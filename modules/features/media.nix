{ ... }: {
  flake.types.generic.feature-options.media = { inputs, lib }:
    with lib;
    let inherit (inputs.self.types.generic) media-submodule;
    in mkOption {
      type = media-submodule { inherit inputs lib; };
      default = { };
      description = "Media configuration";
    };

  flake.modules.homeManager.media = { config, lib, pkgs, ... }: {
    config = lib.mkIf config.host.features.media.enable {
      home.packages = with pkgs; [ plex vlc ];
      programs.kodi.enable = false;
    };
  };

  flake.modules.nixos.media-feature = { config, lib, ... }: {
    config = lib.mkIf config.host.features.media.server.enable {
      networking.firewall.allowedTCPPorts = [ 32400 ];
      services.plex.enable = true;
    };
  };
}
