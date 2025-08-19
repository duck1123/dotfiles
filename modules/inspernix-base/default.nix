{ config, inputs, pkgs, ... }: {
  networking = {
    hostName = "inspernix";
  };

  services = {
    avahi = {
      enable = true;
      nssmdns4 = true;
      openFirewall = true;
    };

    gnome.gnome-keyring.enable = true;
    gvfs.enable = true;
    libinput.enable = true;
    upower.enable = true;

    xserver = {
      enable = true;

      xkb = {
        layout = "us";
        variant = "";
      };
    };
  };

  users.users.duck = {
    isNormalUser = true;
    description = "Duck Nebuchadnezzar";
    extraGroups = [ "networkmanager" "wheel" ];
    packages = with pkgs; [ ];
  };

  # Add network file sharing packages
  environment.systemPackages = with pkgs; [
    gvfs
    nfs-utils
    cifs-utils
  ];
}
