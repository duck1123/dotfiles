{ config, lib, ... }:
let
  inherit (lib) mkOption types;
  cfg = config.features;

  # A module body, or a list of them. A body is a module function/attrset that
  # only sets config; the enable gate is added by `gate` below.
  bodies = types.mkOptionType {
    name = "moduleBodies";
    description = "module body or list of module bodies";
    check = _: true;
    merge = _: defs: lib.concatMap (d: if lib.isList d.value then d.value else [ d.value ]) defs;
  };

  # Wrap a body so it only applies when the host enables the feature. The
  # wrapper advertises the body's own function args, so the module system
  # supplies `pkgs`, `inputs`, etc. exactly as if the body were the module.
  # Ungated features (`gated = false`) return their own conditional config.
  gate =
    name: feature: body:
    let
      bodyFn = if lib.isFunction body then body else _: body;
    in
    lib.setFunctionArgs
      (
        { config, lib, ... }@args:
        {
          config =
            if feature.gated then lib.mkIf config.host.features.${name}.enable (bodyFn args) else bodyFn args;
        }
      )
      (
        lib.functionArgs bodyFn
        // {
          config = false;
          lib = false;
        }
      );

  toModule = class: name: feature: {
    _class = class;
    imports = map (gate name feature) feature.${class};
  };

  # modules.<class>.features.<name> for every feature that has that class
  modulesFor =
    class: lib.mapAttrs (toModule class) (lib.filterAttrs (_: feature: feature.${class} != [ ]) cfg);

  classes = [
    "homeManager"
    "nixos"
  ];

  featureType = types.submodule (
    { name, config, ... }:
    {
      options = {
        description = mkOption {
          type = types.str;
          default = "${name} feature";
          description = "Human description used in the generated `enable` option docs.";
        };

        extraOptions = mkOption {
          type = types.functionTo (types.lazyAttrsOf types.raw);
          default = _: { };
          description = ''
            Function `{ inputs, lib }: { <name> = mkOption ...; }` returning options to
            declare next to `enable` in `hosts.<host>.features.<name>`.
          '';
        };

        option = mkOption {
          type = types.nullOr (types.functionTo types.raw);
          default = null;
          description = ''
            Function `{ inputs, lib }: mkOption ...` replacing the whole generated
            `hosts.<host>.features.<name>` option. Use when the feature needs a
            custom submodule type instead of `enable` plus `extraOptions`.
          '';
        };

        gated = mkOption {
          type = types.bool;
          default = true;
          description = ''
            Wrap the module bodies in `mkIf hosts.<host>.features.<name>.enable`. Set
            to false when the feature has no plain `enable` (or gates differently per
            class); its bodies then return their own conditional config.
          '';
        };

        homeManager = mkOption {
          type = bodies;
          default = [ ];
          description = "home-manager module body, applied when the feature is enabled.";
        };

        nixos = mkOption {
          type = bodies;
          default = [ ];
          description = "NixOS module body, applied when the feature is enabled.";
        };

        featureOption = mkOption {
          type = types.functionTo types.raw;
          readOnly = true;
          internal = true;
          description = "The generated option function stored in `types.generic.feature-options`.";
          default =
            if config.option != null then
              config.option
            else
              { inputs, lib }:
              inputs.self.types.generic.simpleFeatureWith { inherit inputs lib; } config.description (
                config.extraOptions { inherit inputs lib; }
              );
        };
      };
    }
  );
in
{
  options.features = mkOption {
    type = types.attrsOf featureType;
    default = { };
    description = ''
      Feature registry. A feature file declares `features.<name>` once and that
      generates the `hosts.<host>.features.<name>` option, the enable-gated
      `modules.{homeManager,nixos}.features.<name>` modules, and (through
      `base`) their inclusion in every host.
    '';
  };

  config = {
    flake.types.generic.feature-options = lib.mapAttrs (_: feature: feature.featureOption) cfg;

    # published as modules.<class>.features.<name> (see nested-modules.nix)
    nestedModules = lib.genAttrs classes (class: {
      features = modulesFor class;
    });
  };
}
