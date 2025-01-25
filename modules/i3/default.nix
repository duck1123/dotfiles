{ inputs, pkgs, ... }: {
  environment.systemPackages = with pkgs; [
    wget
    vim
    neovim
    curl
    cmatrix
    neofetch
    rofi
    xfce.thunar
    htop
    git
    xfce.ristretto
    feh
    lxappearance
    imagemagick
    zip
    jq
    unzip
    qemu_kvm
  ];

  services = {
    displayManager.sddm = {
      enable = true;
      theme = "ocean";
      wayland.enable = true;
    };
    gvfs.enable = true;
    picom = {
      enable = true;
      fade = true;
      # vSync = true;
      shadow = true;
      fadeDelta = 4;
      inactiveOpacity = 0.8;
      activeOpacity = 1;
      # backend = "glx";
      settings = {
        blur = {
          #method = "dual_kawase";
          #  background = true;
          strength = 5;
        };
      };
    };
    tumbler.enable = true;
    xserver.windowManager.i3 = {
      enable = true;
      package = pkgs.i3-gaps;
    };
  };
}
