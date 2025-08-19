{ pkgs, ... }: {
  environment.systemPackages = with pkgs; [ git ];

  services = {
    flatpak.enable = true;
    plex.enable = true;
    udev.packages = with pkgs; [ gnome-settings-daemon ];
  };

  virtualisation = {
    docker.enable = true;
    libvirtd.enable = true;
  };
}
