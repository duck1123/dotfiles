{
  description = "Sims 4 saves and mods: the `sims` Nushell commands (nushell/modules/sims_module.nu) and the simsnix framework (modules/sims4/_simsnix)";

  extraOptions =
    { lib, ... }:
    with lib;
    {
      backupDir = mkOption {
        type = types.str;
        default = "~/.local/share/simsnix/saves";
        description = "Where `sims save backup` writes backups. Keep it on the same filesystem as the instances so Mods snapshots can hardlink.";
      };

      dataDir = mkOption {
        type = types.str;
        default = "~/projects/dotfiles-private/sims4";
        description = "The private catalog and config (a checkout of the private repo's `sims4/`). Read in place by `sims`; never imported by this flake.";
      };

      blobDir = mkOption {
        type = types.str;
        default = "~/.local/share/simsnix/blobs";
        description = "Mod files by SHA-256. Must be on the same filesystem as the instances so they can be hardlinked into Mods/.";
      };

      stateDir = mkOption {
        type = types.str;
        default = "~/.local/share/simsnix/state";
        description = "Files mods write (config, settings), per instance and mod, hardlinked into Mods/ while the mod is enabled.";
      };

      inbox = mkOption {
        type = types.str;
        default = "~/Downloads/sims";
        description = "Folder new mod downloads are dropped into for `sims add`.";
      };

      defaultInstance = mkOption {
        type = types.nullOr types.str;
        default = null;
        description = "Instance used when a command isn't given --instance.";
      };

      instances = mkOption {
        type = types.attrsOf (
          types.submodule {
            options = {
              documents = mkOption {
                type = types.str;
                description = "The instance's `Documents/Electronic Arts/The Sims 4` folder (inside its Wine prefix).";
              };
              gameDir = mkOption {
                type = types.str;
                description = "The instance's install folder, holding `Game/` and `Data/`.";
              };
            };
          }
        );
        default = { };
        description = "Installs of the game, by name. Paths may start with `~/`.";
      };
    };

  homeManager =
    { config, pkgs, ... }:
    let
      cfg = config.host.features.sims4;
    in
    {
      home.packages = [ pkgs.rsync ];

      xdg.configFile."simsnix/config.json".text = builtins.toJSON {
        inherit (cfg)
          backupDir
          blobDir
          dataDir
          defaultInstance
          inbox
          instances
          stateDir
          ;
        # What `sims manifest` evaluates the catalog with
        framework = "${../modules/sims4/_simsnix}";
        # just nixpkgs' lib/, not the whole source (~200 MiB) in the closure
        nixpkgsLib = "${builtins.path {
          name = "nixpkgs-lib";
          path = pkgs.path + "/lib";
        }}";
      };
    };
}
