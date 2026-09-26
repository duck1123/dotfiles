# Registry for `hosts.<name>`, loaded from ../../hosts by ./registries.nix.
#
# A host file holds the host's data (the `hostSubmodule` options: features,
# environments, identity, ...) plus its own modules:
#
#   modules.homeManager  -> modules.homeManager.<name> + homeConfigurations
#   modules.nixos        -> modules.nixos.<name> + nixosConfigurations (when
#                           `nixos.enable`)
#
# The generated nixos/home-manager configurations set `host =
# config.hosts.<name>`; anything else importing modules.<class>.<name> must set
# it itself. The modules are published as-is (not wrapped), so merge order
# matches a module written directly in flake.modules. The data is
# evaluated once here and published as `inputs.self.hosts`; generic modules see
# it read-only as `config.hosts` (modules/options/hosts-options.nix).
{
  config,
  inputs,
  lib,
  ...
}:
let
  inherit (lib) mkOption types;
  cfg = config.hosts;

  hostSubmodule = config.flake.types.generic.hostSubmodule { inherit inputs lib; };

  # Options that only exist in the registry, stripped from the published data.
  registryOnly = [
    "homeConfigurationName"
    "modules"
  ];

  registryModule =
    { name, config, ... }:
    {
      options = {
        modules = {
          homeManager = mkOption {
            type = types.nullOr types.raw;
            default = null;
            description = "home-manager module for this host.";
          };

          nixos = mkOption {
            type = types.nullOr types.raw;
            default = null;
            description = "NixOS module for this host.";
          };
        };

        homeConfigurationName = mkOption {
          type = types.str;
          default = "${config.identity.username}@${config.hostname}";
          description = "Attribute name under `homeConfigurations`.";
        };
      };

      config = {
        hostname = lib.mkDefault name;
        name = lib.mkDefault config.hostname;
      };
    };

  registryType = types.submoduleWith {
    modules = hostSubmodule.getSubModules ++ [ registryModule ];
    shorthandOnlyDefinesConfig = true;
  };

  withModule = class: lib.filterAttrs (_: host: host.modules.${class} != null) cfg;

  # Points `host` at the host's published data.
  setHost =
    name:
    { config, ... }:
    {
      host = config.hosts.${name};
    };

  pkgsFor = lib.genAttrs config.systems (
    system:
    import inputs.nixpkgs {
      inherit system;
      config.allowUnfree = true;
    }
  );
in
{
  options.hosts = mkOption {
    type = types.attrsOf registryType;
    default = { };
    description = "Host registry (see hosts/).";
  };

  config.flake = {
    hosts = lib.mapAttrs (_: host: removeAttrs host registryOnly) cfg;

    modules = {
      homeManager = lib.mapAttrs (_: host: host.modules.homeManager) (withModule "homeManager");
      nixos = lib.mapAttrs (_: host: host.modules.nixos) (withModule "nixos");
    };

    nixosConfigurations = lib.mapAttrs (
      name: host:
      (inputs.self.lib.mk-os.mkNixos host.system "nixos" name).extendModules {
        modules = [ (setHost name) ];
      }
    ) (lib.filterAttrs (_: host: host.nixos.enable) (withModule "nixos"));

    homeConfigurations = lib.mapAttrs' (
      name: host:
      lib.nameValuePair host.homeConfigurationName (
        inputs.home-manager.lib.homeManagerConfiguration {
          pkgs = pkgsFor.${host.system};
          extraSpecialArgs = {
            inherit inputs;
            pkgs = pkgsFor.${host.system};
          };
          modules = [
            inputs.self.modules.homeManager.base
            inputs.self.modules.homeManager.${name}
            (setHost name)
          ];
        }
      )
    ) (withModule "homeManager");
  };
}
