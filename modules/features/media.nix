{ ... }:
let
  feature-name = "media";
in
{
  flake.types.generic.feature-options.${feature-name} =
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

  flake.modules.homeManager.${feature-name} =
    {
      config,
      lib,
      pkgs,
      ...
    }:
    {
      config = lib.mkIf config.host.features.${feature-name}.enable {
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
    };

  flake.modules.nixos.${feature-name} =
    { config, lib, ... }:
    let
      inherit (config.host.identity) username;
      primaryGroup = config.users.users.${username}.group;
    in
    {
      config = lib.mkIf config.host.features.${feature-name}.server.enable {
        networking.firewall.allowedTCPPorts = [ 32400 ];

        services.plex = {
          enable = true;
          group = primaryGroup;
          openFirewall = true;
          user = username;
        };
      };
    };
}
