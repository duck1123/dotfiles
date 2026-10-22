{
  description = "Media configuration";

  # HM gates on `enable`, NixOS on `server.enable`, so each body gates itself
  gated = false;

  option =
    { inputs, lib }:
    with lib;
    let
      inherit (inputs.self.types.generic) media-submodule;
    in
    mkOption {
      type = media-submodule { inherit inputs lib; };
      default = { };
      description = "Media configuration";
    };

  homeManager =
    {
      config,
      inputs,
      lib,
      pkgs,
      ...
    }:
    let
      inherit (config.host.features) media;
      selfPkgs = inputs.self.packages.${pkgs.stdenv.hostPlatform.system};
    in
    lib.mkMerge [
      (lib.mkIf media.enable {
        home.packages = with pkgs; [
          selfPkgs.caldera-amp
          cozy
          ffmpeg
          mpv
          playerctl
          plex
          plexamp
          vlc
          yt-dlp
          youtube-tui
        ];
        programs.kodi.enable = false;
      })

      # Headless Caldera Music player, cast to from Plexamp. A user service so
      # it plays through the user's PipeWire; log in once with
      # `caldera-music --login --player-name <name>` before starting it.
      (lib.mkIf media.server.enable {
        home.packages = [ selfPkgs.caldera-music ];

        systemd.user.services.caldera-music = {
          Unit = {
            Description = "Caldera Music Player";
            After = [
              "network-online.target"
              "pipewire.service"
            ];
            Wants = [ "network-online.target" ];
            ConditionPathExists = "%h/.config/caldera-music/preferences.json";
          };
          Service = {
            ExecStart = "${lib.getExe selfPkgs.caldera-music} --config %h/.config/caldera-music";
            Restart = "on-failure";
            RestartSec = 5;
          };
          Install.WantedBy = [ "default.target" ];
        };
      })
    ];

  nixos =
    { config, lib, ... }:
    let
      inherit (config.host.identity) username;
      primaryGroup = config.users.users.${username}.group;
    in
    lib.mkIf config.host.features.media.server.enable {
      networking.firewall = {
        # Plex, and the Caldera Music companion's HTTP port
        allowedTCPPorts = [
          32400
          32500
        ];
        # Caldera Music: Plex GDM discovery and Xita multi-room audio
        allowedUDPPorts = [
          32412
          9999
        ];
      };

      # Start the user's caldera-music service at boot, without a login
      users.users.${username}.linger = true;

      services.plex = {
        enable = true;
        group = primaryGroup;
        openFirewall = true;
        user = username;
      };

      system.activationScripts.plexStatePermissions.text = ''
        if [ -d /var/lib/plex ] && [ "$(stat -c '%U:%G' /var/lib/plex)" != "${username}:${primaryGroup}" ]; then
          chown -R ${username}:${primaryGroup} /var/lib/plex
        fi
      '';
    };
}
