{
  description = "Sims 4 save management: the `sims` Nushell commands (nushell/modules/sims_module.nu)";

  extraOptions =
    { lib, ... }:
    with lib;
    {
      backupDir = mkOption {
        type = types.str;
        default = "~/.local/share/simsnix/saves";
        description = "Where `sims save backup` writes backups. Keep it on the same filesystem as the instances so Mods snapshots can hardlink.";
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
        inherit (cfg) backupDir defaultInstance instances;
      };
    };
}
