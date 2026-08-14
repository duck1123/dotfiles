_: {
  flake.modules.nixos.nix-feature-attic =
    {
      config,
      inputs,
      lib,
      pkgs,
      ...
    }:
    let
      atticClient = inputs.attic.packages.${pkgs.stdenv.hostPlatform.system}.attic-client;
      atticEndpoint = "https://attic.home.kronkltd.net";
      atticCache = "nixos";
    in
    {
      config = lib.mkIf (config.host.features.nix.enable && config.host.features.nix.atticPush.enable) {
        sops.secrets.attic-push-token = {
          sopsFile = ./../../secrets/attic-token.yaml;
          key = "attic_push_token";
          path = "/run/secrets/attic-push-token";
          mode = "0400";
          owner = "duck";
          group = "users";
          restartUnits = [ "attic-watch-store.service" ];
        };

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
            attic login ${atticCache} ${atticEndpoint} "$(cat ${config.sops.secrets.attic-push-token.path})"
            exec attic watch-store ${atticCache}
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
