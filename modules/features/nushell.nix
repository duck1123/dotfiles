{ ... }: {
  flake.modules.homeManager.nushell = { config, lib, pkgs, ... }: {
    config = lib.mkIf config.host.features.nushell.enable {
      home.packages = with pkgs; [
        carapace
        fish
        nushell
        nu_scripts
        nufmt
        # nushellPlugins.highlight
        nushellPlugins.formats
        nushellPlugins.polars
        nushellPlugins.gstat
        # nushellPlugins.units
        nushellPlugins.query
        # nushellPlugins.dbus
        nushellPlugins.skim
        # nushellPlugins.net
      ];

      home.file."nushell/completions".source = ../../nushell/completions;
      home.file."nushell/modules".source = ../../nushell/modules;

      home.file."nushell/me.nu".source = builtins.fetchurl {
        url =
          "https://raw.githubusercontent.com/ClipplerBlood/me.nu/09e4ee7fbee6a26cb8dd3041e9da1f4de2c8d119/me.nu";
        sha256 = "sha256:1l3mhcwl2mvkz9qg3yzgz7xdrkdr4xzxfxj3mhv22knbaca5dacr";
      };

      programs = {
        carapace = {
          enable = true;
          enableNushellIntegration = true;
        };

        nushell = {
          enable = true;
          configFile.source = ../../nushell/config.nu;
          envFile.source = ../../nushell/env.nu;

          extraEnv = ''
            $env.EDITOR = "emacsclient -c -a \'\'";
            $env.VISUAL = "emacsclient -c -a \'\'";
          '';

          shellAliases = {
            d = "devspace";
            dr = "devspace run";
            cat = "bat";
            hh = "hstr";
            bbg = "bb --config ~/.bb/bb.edn";
            # psgrep = "ps -ef | grep -v grep | grep ";
            rmr = "runme run";
          };
        };
      };
    };
  };
}
