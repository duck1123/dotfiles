# simsnix option declarations: mods (by creator, then mod), profiles, the
# active profile and the computed manifest. Evaluated by ./eval.nix with
# lib.evalModules, never as part of a NixOS/home-manager config.
#
# A mod's id is "<creator>/<mod>" and its options live at
# sims4.mods.<creator>.<mod>. A mod definition is a submodule, so it can
# declare options of its own (e.g. a variant choice) and use them to set
# `include`/`exclude`.
#
# Don't name a mod option `options`, `config` or `imports`: ./sources.nix
# treats an attrset with those keys as a module.
{ config, lib, ... }:
let
  inherit (lib) mkOption types;
  top = config;

  member = types.submodule {
    options = {
      path = mkOption {
        type = types.str;
        description = "Path inside the archive.";
      };
      sha256 = mkOption {
        type = types.str;
        description = "SHA-256 of the member (hex), the name of its blob.";
      };
    };
  };

  file = types.submodule {
    options = {
      name = mkOption {
        type = types.str;
        description = "File name as downloaded.";
      };
      sha256 = mkOption {
        type = types.str;
        description = "SHA-256 of the file (hex).";
      };
      members = mkOption {
        type = types.listOf member;
        default = [ ];
        description = "For an archive, its members; they are installed instead of the archive.";
      };
    };
  };

  version = types.submodule {
    options = {
      added = mkOption {
        type = types.nullOr types.str;
        default = null;
        description = "Date the version was recorded (YYYY-MM-DD).";
      };
      gameVersion = mkOption {
        type = types.nullOr types.str;
        default = null;
        description = "Game version the mod was made for, if known.";
      };
      files = mkOption {
        type = types.listOf file;
        default = [ ];
        description = "The files that make up this version.";
      };
    };
  };

  mod =
    creator:
    { name, config, ... }:
    {
      options = {
        id = mkOption {
          type = types.str;
          readOnly = true;
          default = "${creator}/${name}";
          description = "The mod's id, `<creator>/<mod>`.";
        };
        enable = mkOption {
          type = types.bool;
          default = lib.elem config.id top.sims4.activeMods;
          defaultText = "whether the active profile lists the mod";
          description = "Install this mod. Defaults to whether the active profile lists it.";
        };
        title = mkOption {
          type = types.nullOr types.str;
          default = null;
          description = "Display name.";
        };
        downloadPage = mkOption {
          type = types.nullOr types.str;
          default = null;
        };
        tags = mkOption {
          type = types.listOf types.str;
          default = [ ];
        };
        folder = mkOption {
          type = types.nullOr types.str;
          default = "${creator}_${name}";
          description = "Folder under Mods/ the files go in; null puts them directly in Mods/.";
        };
        stripComponents = mkOption {
          type = types.ints.unsigned;
          default = 0;
          description = "Leading path components to drop from archive members (like tar).";
        };
        include = mkOption {
          type = types.listOf types.str;
          default = [
            ".*\\.package"
            ".*\\.ts4script"
          ];
          description = "Regexes (whole-path match, against the lowercased path) of files to install.";
        };
        exclude = mkOption {
          type = types.listOf types.str;
          default = [ ];
          description = "Regexes (as for `include`) of files not to install.";
        };
        stateFiles = mkOption {
          type = types.listOf types.str;
          default = [ ];
          description = "Globs, relative to the mod's folder, of files the mod writes. Kept in the state store and hardlinked in.";
        };
        requires = mkOption {
          type = types.listOf types.str;
          default = [ ];
          description = "Ids of mods that must be enabled with this one.";
        };
        conflicts = mkOption {
          type = types.listOf types.str;
          default = [ ];
          description = "Ids of mods that must not be enabled with this one.";
        };
        versions = mkOption {
          type = types.attrsOf version;
          default = { };
          description = "Known versions, by label. Merged across sources.";
        };
        version = mkOption {
          type = types.nullOr types.str;
          default = null;
          description = "Version to install; null = the highest label (builtins.compareVersions).";
        };
        selectedVersion = mkOption {
          type = types.nullOr types.str;
          readOnly = true;
          default =
            if config.version != null then
              config.version
            else
              let
                labels = lib.sort (a: b: builtins.compareVersions a b > 0) (lib.attrNames config.versions);
              in
              if labels == [ ] then null else lib.head labels;
          description = "The version that will be installed.";
        };
      };
    };

  creator = types.submodule (
    { name, ... }:
    {
      freeformType = types.attrsOf (types.submodule (mod name));
    }
  );
in
{
  options.sims4 = {
    mods = mkOption {
      type = types.attrsOf creator;
      default = { };
      description = "Mods, as sims4.mods.<creator>.<mod>.";
    };

    profiles = mkOption {
      type = types.attrsOf (
        types.submodule {
          options = {
            base = mkOption {
              type = types.nullOr types.str;
              default = null;
              description = "Profile to start from.";
            };
            mods = mkOption {
              type = types.listOf types.str;
              default = [ ];
              description = "Mod ids to add.";
            };
            remove = mkOption {
              type = types.listOf types.str;
              default = [ ];
              description = "Mod ids to drop from the base.";
            };
          };
        }
      );
      default = { };
    };

    profile = mkOption {
      type = types.nullOr types.str;
      default = null;
      description = "The active profile (set by the `sims` tool). null = no mod enabled by default.";
    };

    activeMods = mkOption {
      type = types.listOf types.str;
      readOnly = true;
      description = "Mod ids the active profile resolves to (profiles.nix).";
    };

    assertions = mkOption {
      type = types.listOf types.unspecified;
      internal = true;
      default = [ ];
      description = "{ assertion, message } pairs checked by eval.nix.";
    };

    manifest = mkOption {
      type = types.unspecified;
      readOnly = true;
      description = "What `sims switch` installs; see manifest.nix.";
    };
  };
}
