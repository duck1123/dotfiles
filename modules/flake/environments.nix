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
    homeManager =
      name: config:
      let
        envs = config.host.environments;
      in
      envs.primary == name || envs.${name}.enable;
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
      types.generic.environments-submodule =
        assert lib.assertMsg (!(cfg ? primary)) "`primary` is reserved and can't name an environment";
        hostEnvironmentsSubmodule;

      modules.nixos.specialisations =
        { config, inputs, ... }:
        let
          envs = config.host.environments;
          specialised = builtins.filter (name: name != envs.primary && envs.${name}.enable) names;
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
