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
      lib,
      pkgs,
      ...
    }:
    lib.mkIf config.host.features.media.enable {
      home.packages = with pkgs; [
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
    };

  nixos =
    { config, lib, ... }:
    let
      inherit (config.host.identity) username;
      primaryGroup = config.users.users.${username}.group;
    in
    lib.mkIf config.host.features.media.server.enable {
      networking.firewall.allowedTCPPorts = [ 32400 ];

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
