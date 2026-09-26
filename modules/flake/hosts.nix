# Registry for `hosts.<name>`, loaded from ../../hosts by ./registries.nix.
#
# `info` is the host's JSON-friendly summary, published as
# `inputs.self.hostInfo` and written to ~/.config/dotfiles/hosts.json by
# `modules.homeManager.host-info` (read by nushell/modules/hosts_module.nu).
# Its fields default to `defaultInfo` below; a host file can override or add
# any of them with `info.<field> = ...`.
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
    "info"
    "modules"
  ];

  # A feature counts as enabled when its `enable` is set, or (for features with
  # a custom option like kubernetes) any direct sub-option's `enable` is.
  isEnabled =
    feature:
    feature.enable or false
    || lib.any (v: lib.isAttrs v && (v.enable or false) == true) (builtins.attrValues feature);

  # What gets recorded about each host. Keep it JSON-serialisable: no paths
  # (they'd be copied to the store) or functions.
  defaultInfo = host: {
    inherit (host) hostname name system;
    user = host.identity.username;
    nixos = host.nixos.enable;
    android = host.android.enable;
    environments = {
      inherit (host.environments) primary;
      used = inputs.self.lib.environments.usedBy host.environments;
    };
    features = builtins.attrNames (lib.filterAttrs (_: isEnabled) host.features);
  };

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

        info = mkOption {
          type = types.attrsOf types.anything;
          description = "JSON summary of the host (see `defaultInfo`).";
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
        info = lib.mapAttrs (_: lib.mkDefault) (defaultInfo config);
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
    hostInfo = lib.mapAttrs (_: host: host.info) cfg;

    modules = {
      homeManager = lib.mapAttrs (_: host: host.modules.homeManager) (withModule "homeManager") // {
        # Every host's `info`, for nushell (hosts_module.nu) and other tools.
        # Imported by homeManager.base.
        host-info =
          { config, inputs, ... }:
          {
            xdg.configFile."dotfiles/hosts.json".text = builtins.toJSON {
              current = config.host.hostname;
              hosts = builtins.attrValues inputs.self.hostInfo;
            };
          };
      };
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
