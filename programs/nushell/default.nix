{ pkgs, ... }: {
  home.packages = with pkgs; [
    carapace
    fish
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

  home.file."nushell/completions".source = ./completions;
  home.file."nushell/modules".source = ./modules;

  home.file."nushell/me.nu".source = builtins.fetchurl {
    url = "https://raw.githubusercontent.com/ClipplerBlood/me.nu/09e4ee7fbee6a26cb8dd3041e9da1f4de2c8d119/me.nu";
    sha256 = "sha256:1l3mhcwl2mvkz9qg3yzgz7xdrkdr4xzxfxj3mhv22knbaca5dacr";
  };

  programs = {
    carapace = {
      enable = true;
      enableNushellIntegration = true;
    };

    nushell = {
      enable = true;
      configFile.source = ./config.nu;
      envFile.source = ./env.nu;

      extraConfig = ''
        let carapace_completer = {|spans|
          carapace $spans.0 nushell ...$spans
            | from json
            | if ($in | default [] | where value == $"($spans | last)ERR" | is-empty) { $in } else { null }
        }

        let fish_completer = {|spans|
          fish --private -i --command $'complete --do-complete "($spans | str join " ")"'
            | from tsv --flexible --noheaders --no-infer
            | rename value description
            | update cells --columns ["value"] { ansi strip }
        }

        # This completer will use carapace by default
        let external_completer = {|spans|
          let expanded_alias = scope aliases
            | where name == $spans.0
            | get -i 0.expansion

          let spans = if $expanded_alias != null {
              $spans
                | skip 1
                | prepend ($expanded_alias | split row ' ' | take 1)
            } else {
              $spans
            }

          match $spans.0 {
            ag => $fish_completer
            alacritty => $fish_completer
            asdf => $fish_completer
            argocd => $fish_completer
            az => $fish_completer
            doctl => $fish_completer
            git => $fish_completer
            jj => $fish_completer
            k3d => $fish_completer
            keepassxc-cli => $fish_completer
            # mc => $fish_completer
            nu => $fish_completer
            playerctl => $fish_completer
            sops => $fish_completer
            tailscale => $fish_completer
            _ => $carapace_completer
          } | do $in $spans
        }

        $env.config.completions.external = { enable: true completer: $external_completer }
        '';

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

    starship = {
      enable = true;
      # enableZshIntegration = false;

      settings = {
        add_newline = true;

        format =
          "$shlvl$username$hostname$kubernetes$git_branch$git_commit$git_state$git_status$directory$nix_shell$jobs\n$cmd_duration$shell$character";

        character = {
          success_symbol = "[\\$](bright-green bold)";
          error_symbol = "[\\$](bright-red bold)";
        };

        cmd_duration = {
          format = "[$duration]($style) ";
          style = "bright-blue";
        };

        directory = {
          format = "[$path]($style)[$read_only]($read_only_style)";
          read_only = "";
          truncation_length = 0;
        };

        git_branch = {
          only_attached = true;
          format = "[$symbol$branch]($style)";
          symbol = "שׂ";
          style = "bright-yellow bold";
        };

        git_commit = {
          only_detached = true;
          format = "[ﰖ$hash]($style) ";
          style = "bright-yellow bold";
        };

        git_state = { style = "bright-purple bold"; };
        git_status = { style = "bright-yellow bold"; };

        hostname = {
          disabled = false;
          style = "bright-green bold";
          # ssh_only = true;
        };

        jobs = { style = "bright-green bold"; };

        kubernetes = {
          disabled = false;
          format = "[$symbol$context(\\[$namespace\\])]($style) ";
          style = "purple bold";
        };

        nix_shell = {
          symbol = "❄️";
          format = "[$symbol]($style) ";
          style = "bright-purple bold";
        };

        shell = {
          disabled = false;
          format = "[\\[](bright-purple bold)$indicator[\\]](bright-purple bold)";
          fish_indicator = "[🐟](bright-blue)";
          nu_indicator = "[Ν](bright-blue)";
          bash_indicator = "[ϐ](bright-blue)";
          zsh_indicator = "[Ζ](bright-blue)";
        };

        shlvl = {
          disabled = false;
          format = "[$symbol$shlvl]($style)";
          symbol = "ﰬ";
          style = "bright-red bold";
        };

        username = {
          style_user = "bright-white bold";
          style_root = "bright-red bold";
        };
      };
    };
  };
}
