_: {
  flake.modules.nixos.nix-attic =
    {
      config,
      inputs,
      lib,
      pkgs,
      ...
    }:
    let
      atticClient = inputs.attic.packages.${pkgs.stdenv.hostPlatform.system}.attic-client;
      attic = {
        endpoint = "https://attic.home.kronkltd.net";
        cache = "nixos";
      };
      cachix = {
        cache = "duck1123";
      };
      inherit (config.host.features) nix;
    in
    {
      config = lib.mkIf (nix.enable && nix.atticPush.enable) {
        sops.secrets = {
          attic-push-token = {
            sopsFile = ./../../secrets/attic-token.yaml;
            key = "attic_push_token";
            path = "/run/secrets/attic-push-token";
            mode = "0400";
            owner = "duck";
            group = "users";
            restartUnits = [ "attic-watch-store.service" ];
          };
        }
        // lib.optionalAttrs nix.cachixPush.enable {
          cachix-push-token = {
            sopsFile = ./../../secrets/cachix-token.yaml;
            key = "cachix_push_token";
            path = "/run/secrets/cachix-push-token";
            mode = "0400";
            owner = "duck";
            group = "users";
            restartUnits = [ "cachix-watch-store.service" ];
          };
        };

        # Kept installed without cachixPush, for manual `cachix push`.
        environment.systemPackages = [ pkgs.cachix ];

        systemd.services.attic-watch-store = {
          description = "Auto-push newly built store paths to the Attic cache";
          after = [
            "network-online.target"
            "sops-nix.service"
          ];
          wants = [ "network-online.target" ];
          wantedBy = [ "multi-user.target" ];

          path = [ atticClient ];

          script = ''
            attic login ${attic.cache} ${attic.endpoint} "$(cat ${config.sops.secrets.attic-push-token.path})"
            exec attic watch-store ${attic.cache}
          '';

          serviceConfig = {
            User = "duck";
            Restart = "on-failure";
            RestartSec = "10s";
          };
        };

        systemd.services.cachix-watch-store = lib.mkIf nix.cachixPush.enable {
          description = "Auto-push newly built store paths to the ${cachix.cache} Cachix cache";
          after = [
            "network-online.target"
            "sops-nix.service"
          ];
          wants = [ "network-online.target" ];
          wantedBy = [ "multi-user.target" ];

          path = [ pkgs.cachix ];

          script = ''
            export CACHIX_AUTH_TOKEN="$(cat ${config.sops.secrets.cachix-push-token.path})"
            exec cachix watch-store ${cachix.cache}
          '';

          serviceConfig = {
            User = "duck";
            Restart = "on-failure";
            RestartSec = "10s";
          };
        };
      };
    };
}
