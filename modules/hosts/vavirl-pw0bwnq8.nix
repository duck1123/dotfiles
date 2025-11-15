{ ... }:
let
  hostname = "vavirl-pw0bwnq8";
  loadHosts = config: import ../../hosts/default.nix { inherit config; };
in {
  flake.modules = {
    homeManager.${hostname} = { config, pkgs, ... }:
      let
        hosts = loadHosts config;
        host = hosts.${hostname};
        inherit (config.host.identity) email gpgKey name username;
      in {
        inherit host hosts;

        programs.home-manager.enable = true;

        home = {
          stateVersion = "21.11";

          username = "${username}";
          homeDirectory = "/home/${username}";

          file.".bb/bb.edn".source = ../../bb.edn;

          packages = with pkgs; [
            bat
            direnv
            git
            gitu
            htop
            neofetch
            nixfmt-classic
            nh
            silver-searcher
            sqlcmd
          ];
        };

        programs = {
          bash.enable = true;

          direnv = {
            enable = true;
            nix-direnv.enable = true;
          };

          eza.enable = true;

          git = {
            enable = true;
            lfs.enable = true;

            settings.user = { inherit email name; };

            signing = {
              signByDefault = false;
              key = gpgKey;
            };
          };

          hstr.enable = true;
          jq.enable = true;
          tmux.enable = true;
        };
      };
  };
}
