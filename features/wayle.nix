let
  captureTokenPath = "/run/secrets/windmill-capture-token";
in
{
  homeManager =
    {
      config,
      inputs,
      lib,
      pkgs,
      ...
    }:
    let
      # Environments that pull wayle in (see `environments.<name>.features`).
      # One home-manager generation serves every specialisation, so the
      # service is limited to their sessions at runtime instead.
      desktops = inputs.self.lib.environments.desktopsFor config.host "wayle";

      # The quick-capture button sends its text to the Windmill `capture` script
      # (f/fleetops/capture), which files it as its own #inbox note under
      # today's day note. Its token is installed by the nixos body below, only
      # on hosts with the claude feature, the ones whose host keys decrypt it.
      quickCaptureEnabled = (config.host.features.claude or { enable = false; }).enable;

      quickCapture = pkgs.writeShellApplication {
        name = "trilium-quick-capture";
        runtimeInputs = with pkgs; [
          coreutils
          curl
          fuzzel
          jq
          libnotify
        ];
        text = ''
          url=https://windmill.home.kronkltd.net/api/w/default/jobs/run_wait_result/p/f/fleetops/capture
          token=$(cat ${captureTokenPath})
          trap 'notify-send -u critical "Quick capture failed" "Could not send to Trilium"' ERR

          text=$(fuzzel --dmenu --prompt-only "Capture: " --placeholder "Thought, link, or task: / watch: / shopping:" --width 60 </dev/null) || exit 0
          text=$(printf '%s' "$text" | sed -E 's/^[[:space:]]+|[[:space:]]+$//g')
          [ -n "$text" ] || exit 0

          result=$(jq -n --arg t "$text" '{text: $t}' \
            | curl -fsS -X POST -H "Authorization: Bearer $token" -H 'Content-Type: application/json' --data-binary @- "$url")

          notify-send "Captured to inbox" "$(jq -r '"\(.title) (\(.kind))"' <<<"$result")"
        '';
      };
    in
    {
      systemd.user.services.wayle.Unit.ConditionEnvironment = lib.mkIf (desktops != [ ]) (
        lib.mkForce ([ "WAYLAND_DISPLAY" ] ++ map (desktop: "|XDG_CURRENT_DESKTOP=${desktop}") desktops)
      );

      # https://github.com/nix-community/home-manager/blob/master/modules/services/wayle.nix
      services.wayle = {
        enable = true;

        settings = {
          # font + palette are managed by stylix's wayle target
          bar = {
            bg = "transparent";
            border-location = "bottom";
            button-icon-padding = 0.5;
            button-icon-size = 0.8;
            button-label-size = 0.8;

            layout = [
              {
                monitor = "*";

                left = [
                  "dashboard"
                  "niri-workspaces"
                  "hyprland-workspaces"
                  "window-title"
                ];

                center = [
                  "clock"
                  "weather"
                  "notifications"
                ];

                right =
                  let
                    batteryEnabled = (config.host.features.battery or { enable = false; }).enable;
                  in
                  (lib.optionals quickCaptureEnabled [ "custom-quick-capture" ])
                  ++ [
                    "media"
                    "network"
                    "bluetooth"
                  ]
                  ++ (lib.optionals batteryEnabled [ "battery" ])
                  ++ [
                    "netstat"
                    "microphone"
                    "volume"
                    "systray"
                  ];
              }
            ];
          };

          modules = {
            bluetooth.label-show = false;

            custom = lib.optionals quickCaptureEnabled [
              {
                id = "quick-capture";
                command = "true";
                interval-ms = 0; # static button: run once, never poll
                icon-name = "ld-file-text-symbolic";
                label-show = false;
                tooltip-format = "Quick capture to Trilium";
                left-click = lib.getExe quickCapture;
              }
            ];

            clock = {
              format = "%b %d %I:%M:%S %p";
              icon-show = false;
              dropdown-show-seconds = false;
            };

            dashboard.icon-bg-color = "bg-overlay";
            media.label-show = false;
            microphone.label-show = false;
            network.label-show = false;

            # Icon-only bell; the popup drops down from the clock.
            notifications = {
              border-color = "transparent";
              icon-color = "bg-overlay";
              icon-bg-color = "transparent";
              label-show = false;
              label-color = "transparent";
              label-max-length = 1;
              button-bg-color = "transparent";
              popup-position = "top-center";
              popup-margin-x = 0.5;
              popup-margin-y = 0.5;
            };
            volume.label-show = false;

            weather = {
              location = "Detroit";
              units = "metric";
            };
          };
        };
      };
    };

  # Token for the quick-capture button: a Windmill token that can only run
  # f/fleetops/capture (windmill.desktopCaptureToken). The k8s secrets file is
  # also encrypted for the server host keys, so sops-nix can install it here.
  nixos =
    { config, lib, ... }:
    {
      sops.secrets.windmill-capture-token =
        lib.mkIf (config.host.features.claude or { enable = false; }).enable
          {
            sopsFile = ./../secrets/k8s.enc.yaml;
            key = "windmill/desktopCaptureToken";
            path = captureTokenPath;
            mode = "0400";
            owner = config.host.identity.username;
            group = "users";
          };
    };
}
