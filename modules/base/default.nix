{ host, pkgs, ... }:
let inherit (host) hostname;
in {
  environment.systemPackages = with pkgs; [ git gvfs zsh ];

  networking = {
    firewall = {
      enable = false;

      allowedTCPPorts = [
        ## barrier port
        24800
        ## Plex Media Server
        32400
      ];
    };

    hostName = "${hostname}"; # Define your hostname.
  };

  services = {
    gvfs.enable = true;
    flatpak.enable = true;
    plex.enable = true;
    udev.packages = with pkgs; [ gnome-settings-daemon ];
  };

  virtualisation = {
    docker.enable = true;
    libvirtd.enable = true;
  };

  xdg.portal = {
    enable = true;
    config.common.default = "*";
  };
}
