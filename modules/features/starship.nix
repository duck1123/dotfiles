{ ... }: {
  flake.modules.homeManager.starship = { config, lib, pkgs, ... }: {
    config = lib.mkIf config.host.features.starship.enable {
      programs = {
        starship = {
          enable = true;
          # enableZshIntegration = false;

          settings = {
            add_newline = true;

            format = ''
              $shlvl$username$hostname$kubernetes$git_branch$git_commit$git_state$git_status$directory$nix_shell$jobs
              $cmd_duration$shell$character'';

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
              format =
                "[\\[](bright-purple bold)$indicator[\\]](bright-purple bold)";
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
    };
  };
}
