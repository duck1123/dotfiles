{ config, lib, ... }:
let
  inherit (lib) mkOption types;
  cfg = config.environments;
  names = builtins.attrNames cfg;

  # A module body, or a list of them (same shape as the feature registry).
  bodies = types.mkOptionType {
    name = "moduleBodies";
    description = "module body or list of module bodies";
    check = _: true;
    merge = _: defs: lib.concatMap (d: if lib.isList d.value then d.value else [ d.value ]) defs;
  };

  # Wrap a body so its config only applies when `cond config` holds. A body's
  # `imports` are hoisted out of the gate, since imports can't be conditional;
  # imported modules must gate themselves (e.g. behind their own `enable`).
  gate =
    cond: body:
    let
      bodyFn = if lib.isFunction body then body else _: body;
    in
    lib.setFunctionArgs
      (
        { config, lib, ... }@args:
        let
          result = bodyFn args;
        in
        {
          imports = result.imports or [ ];
          config = lib.mkIf (cond config) (removeAttrs result [ "imports" ]);
        }
      )
      (
        lib.functionArgs bodyFn
        // {
          config = false;
          lib = false;
        }
      );

  # NixOS: the environment is the one this system (or specialisation) runs.
  # home-manager: shared by every specialisation, so apply it whenever the
  # host uses the environment at all.
  conditions = {
    nixos = name: config: config.environments.active == name;
    homeManager = name: config: builtins.elem name (usedBy config.host.environments);
  };

  toModule = class: name: environment: {
    _class = class;
    imports = map (gate (conditions.${class} name)) environment.${class};
  };

  modulesFor =
    class:
    lib.mapAttrs (toModule class) (lib.filterAttrs (_: environment: environment.${class} != [ ]) cfg);

  classes = builtins.attrNames conditions;

  environmentType = types.submodule (
    { name, ... }:
    {
      options = {
        description = mkOption {
          type = types.str;
          default = "${name} environment";
          description = "Human description used in the generated host option docs.";
        };

        features = mkOption {
          type = types.listOf types.str;
          default = [ ];
          description = ''
            Features this environment turns on (with `mkDefault`) for any host
            that uses it. They stay ordinary features: other environments can
            list them too, and a host can still set them directly.
          '';
        };

        desktopNames = mkOption {
          type = types.listOf types.str;
          default = [ ];
          description = ''
            `XDG_CURRENT_DESKTOP` values of the session, so services from
            `features` can be limited to this environment at runtime (see
            `lib.environments.desktopsFor`).
          '';
        };

        homeManager = mkOption {
          type = bodies;
          default = [ ];
          description = "home-manager module body, applied when the host uses the environment.";
        };

        nixos = mkOption {
          type = bodies;
          default = [ ];
          description = "NixOS module body, applied when the environment is the active one.";
        };
      };
    }
  );

  # Environments a host uses: its primary plus every enabled one.
  usedBy = envs: builtins.filter (name: envs.primary == name || envs.${name}.enable) names;

  # Host module: turn on the features each used environment asks for.
  hostFeatures =
    { config, lib, ... }:
    {
      config.features = lib.mkMerge (
        map (
          name:
          lib.genAttrs cfg.${name}.features (_: {
            enable = lib.mkDefault true;
          })
        ) (usedBy config.environments)
      );
    };

  # hosts.<host>.environments: `primary` plus an `<name>.enable` per environment
  hostEnvironmentsSubmodule =
    { lib, ... }:
    types.submodule {
      options =
        lib.mapAttrs (
          _: environment:
          mkOption {
            type = types.submodule {
              options.enable = mkOption {
                type = types.bool;
                default = false;
                description = "Build a specialisation for the ${environment.description}";
              };
            };
            default = { };
            description = "${environment.description} configuration";
          }
        ) cfg
        // {
          primary = mkOption {
            type = types.nullOr (types.enum names);
            default = null;
            description = ''
              Environment the host boots into by default. Every other enabled
              environment becomes a specialisation.
            '';
          };
        };
    };
in
{
  options.environments = mkOption {
    type = types.attrsOf environmentType;
    default = { };
    description = ''
      Desktop environment registry. An environment file declares
      `environments.<name>` once and that generates the
      `hosts.<host>.environments.<name>.enable` toggle, the gated
      `modules.{homeManager,nixos}.environments.<name>` modules, and (through
      `nixos.base`) a specialisation on every host that enables it.
    '';
  };

  config = {
    flake = {
      lib.environments = {
        inherit usedBy;

        # XDG_CURRENT_DESKTOP values of the host's environments that list
        # `feature`, for gating that feature's services at runtime.
        desktopsFor =
          host: feature:
          lib.concatMap (
            name: lib.optionals (builtins.elem feature cfg.${name}.features) cfg.${name}.desktopNames
          ) (usedBy host.environments);
      };

      # A module, not a type: types.generic is `anything`, which would wrap it.
      modules.generic.environments-host = hostFeatures;

      types.generic.environments-submodule =
        assert lib.assertMsg (!(cfg ? primary)) "`primary` is reserved and can't name an environment";
        hostEnvironmentsSubmodule;

      modules.nixos.specialisations =
        { config, inputs, ... }:
        let
          envs = config.host.environments;
          specialised = builtins.filter (name: name != envs.primary) (usedBy envs);
        in
        {
          imports = builtins.attrValues inputs.self.modules.nixos.environments;

          options.environments.active = mkOption {
            type = types.nullOr (types.enum names);
            default = envs.primary;
            description = "Environment this system (or specialisation) runs.";
          };

          # Specialisations inherit the whole host config and only swap the
          # active environment; nixpkgs drops nested specialisations itself.
          config.specialisation = lib.genAttrs specialised (name: {
            configuration.environments.active = lib.mkForce name;
          });
        };
    };

    # published as modules.<class>.environments.<name> (see nested-modules.nix)
    nestedModules = lib.genAttrs classes (class: {
      environments = modulesFor class;
    });
  };
}
