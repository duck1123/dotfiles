{ pkgs, ... }: {
  home.packages = with pkgs; [
    carapace
    nushell
    nu_scripts
    nufmt
    nushellPlugins.highlight
    nushellPlugins.formats
    nushellPlugins.polars
    nushellPlugins.gstat
    nushellPlugins.units
    nushellPlugins.query
    nushellPlugins.dbus
    nushellPlugins.skim
    nushellPlugins.net
  ];

  programs = {
    carapace = {
      enable = true;
      enableNushellIntegration = true;
    };

    nushell = {
      enable = true;
      configFile.source = ./config.nu;
      extraConfig = ''
        let carapace_completer = {|spans|
          carapace $spans.0 nushell $spans | from json
        }
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

    starship = {
      enable = true;
      enableZshIntegration = false;

      settings = {
        add_newline = true;
        character = {
          success_symbol = "[➜](bold green)";
          error_symbol = "[➜](bold red)";
        };
      };
    };
  };
}
