# `programs.emacs.parts.<name>`: named chunks of Emacs config, each with its
# own packages and elisp, that are composed into `programs.emacs.extraConfig`
# and `extraPackages`. Any home-manager module (a feature body, a host module)
# can add a part, append to one, or turn one off with `enable = false`.
#
# Declared here rather than in features/emacs.nix because feature bodies are
# config-only (the registry wraps them in `mkIf enable`).
_: {
  flake.modules.homeManager.emacs-parts =
    { config, lib, ... }:
    let
      inherit (lib) mkOption types;

      enabledParts = lib.sort (a: b: if a.order == b.order then a.name < b.name else a.order < b.order) (
        lib.filter (part: part.enable) (lib.attrValues config.programs.emacs.parts)
      );
    in
    {
      options.programs.emacs.parts = mkOption {
        type = types.attrsOf (
          types.submodule (
            { name, ... }:
            {
              options = {
                name = mkOption {
                  type = types.str;
                  default = name;
                  readOnly = true;
                  internal = true;
                };

                enable = mkOption {
                  type = types.bool;
                  default = true;
                  description = "Include the ${name} part in the Emacs config.";
                };

                order = mkOption {
                  type = types.int;
                  default = 1000;
                  description = "Position in init.el; lower runs first, ties by name.";
                };

                packages = mkOption {
                  type = types.functionTo (types.listOf types.package);
                  default = _: [ ];
                  description = "`epkgs: [ ... ]` packages this part needs.";
                };

                config = mkOption {
                  type = types.lines;
                  default = "";
                  description = "Elisp for this part.";
                };
              };
            }
          )
        );
        default = { };
        description = "Composable, individually toggleable pieces of the Emacs config.";
      };

      config.programs.emacs = {
        extraConfig = lib.concatMapStringsSep "\n" (part: ";;; ${part.name}\n${part.config}") enabledParts;
        extraPackages = epkgs: lib.concatMap (part: part.packages epkgs) enabledParts;
      };
    };
}
