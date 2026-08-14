_: {
  flake.types.generic.feature-options.waydroid =
    { inputs, lib }:
    let
      inherit (inputs.self.types.generic) simpleFeature;
    in
    simpleFeature { inherit inputs lib; } "waydroid feature";

  flake.modules.nixos.waydroid-feature =
    {
      config,
      lib,
      pkgs,
      ...
    }:
    let
      cfg = config.virtualisation.waydroid;

      # `waydroid prop set` and `waydroid shell` both need the Android
      # container booted, which only happens once a session is started
      # (not at boot). Poll `waydroid status` (pure system-bus call) and
      # reapply on every STOPPED -> RUNNING transition, so this covers
      # the "session died, restarted via waydroid-helper" cycle too.
      # `shell` requires root, and `prop set` needs a reachable session
      # bus (unused otherwise) - both are satisfied by running as root
      # with DBUS_SESSION_BUS_ADDRESS pointed at the session user found
      # in the status output.
      tweaks = pkgs.writeShellApplication {
        name = "waydroid-tweaks";
        runtimeInputs = [ cfg.package ];
        text = ''
          prev_state="STOPPED"

          while true; do
            status="$(waydroid status 2>/dev/null || true)"
            state="$(printf '%s\n' "$status" | awk -F'\t' '/^Session:/{print $2}')"

            if [[ "$state" == "RUNNING" && "$prev_state" != "RUNNING" ]]; then
              uid="$(printf '%s\n' "$status" | awk -F'[()]' '/^Session user:/{print $2}')"
              if [[ -n "$uid" ]]; then
                export DBUS_SESSION_BUS_ADDRESS="unix:path=/run/user/$uid/bus"
                waydroid prop set persist.waydroid.multi_windows true || true
                waydroid shell settings put system screen_off_timeout 2147483647 || true
              fi
            fi

            prev_state="$state"
            sleep 5
          done
        '';
      };
    in
    {
      config = lib.mkIf config.host.features.waydroid.enable {
        # Enable clipboard sharing
        environment.systemPackages = with pkgs; [
          waydroid-helper
          wl-clipboard
        ];

        services.geoclue2.enable = true;

        systemd = {
          packages = with pkgs; [ waydroid-helper ];
          services = {
            waydroid-mount.wantedBy = [ "multi-user.target" ];
            waydroid-tweaks = {
              description = "Enable Waydroid multi-window mode and disable Android screen timeout";
              after = [ "waydroid-container.service" ];
              bindsTo = [ "waydroid-container.service" ];
              wantedBy = [ "waydroid-container.service" ];
              serviceConfig = {
                Type = "simple";
                ExecStart = lib.getExe tweaks;
                Restart = "on-failure";
                RestartSec = 5;
              };
            };
          };
        };

        virtualisation.waydroid = {
          enable = true;
          # Newer kernel versions may need
          package = pkgs.waydroid-nftables;
        };
      };
    };
}
