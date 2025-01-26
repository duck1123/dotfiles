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
      # enableZshIntegration = false;

      settings = {
        add_newline = true;

        format =
          "$shlvl$shell$username$hostname$nix_shell$git_branch$git_commit$git_state$git_status$directory$jobs$cmd_duration\n$character";

        git_branch = {
          only_attached = true;
          format = "[$symbol$branch]($style) ";
          symbol = "שׂ";
          style = "bright-yellow bold";
        };

        git_commit = {
          only_detached = true;
          format = "[ﰖ$hash]($style) ";
          style = "bright-yellow bold";
        };

        hostname = {
          style = "bright-green bold";
          ssh_only = true;
        };

        shlvl = {
          disabled = false;
          symbol = "ﰬ";
          style = "bright-red bold";
        };

        shell = {
          disabled = false;
          format = "$indicator";
          fish_indicator = "[🐟](bright-blue)";
          nu_indicator = "[Ν](bright-blue) ";
          bash_indicator = "[ϐ](bright-blue) ";
          zsh_indicator = "[Ζ](bright-blue) ";
        };

        username = {
          style_user = "bright-white bold";
          style_root = "bright-red bold";
        };

        nix_shell = {
          symbol = "";
          format = "[$symbol$name]($style) ";
          style = "bright-purple bold";
        };

        git_state = { style = "bright-purple bold"; };
        git_status = { style = "bright-green bold"; };
        directory = {
          read_only = " ";
          truncation_length = 0;
        };
        cmd_duration = {
          format = "[$duration]($style) ";
          style = "bright-blue";
        };
        jobs = { style = "bright-green bold"; };

        character = {
          success_symbol = "[\\$](bright-green bold)";
          error_symbol = "[\\$](bright-red bold)";
        };
      };
    };
  };
}
