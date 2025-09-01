{ pkgs, ... }: {
  # hardware.bluetooth.enable = true;

  networking = {
    hostName = "nasnix";
    networkmanager.enable = true;
  };

  programs = {
    # dconf.enable = true;
    # firefox.enable = true;

    # gnupg.agent = {
    #   enable = true;
    #   enableSSHSupport = true;
    # };

    # nix-ld = {
    #   enable = true;
    #   libraries = with pkgs; [
    #     alsa-lib
    #     libGL
    #     # renpy
    #   ];
    # };

    zsh.enable = true;
  };

  security.rtkit.enable = true;

  services = {
    avahi = {
      enable = true;
      nssmdns4 = true;
      openFirewall = true;
    };

    # gnome.gnome-keyring.enable = true;
    gvfs.enable = true;

    # Enable Samba client for Windows/SMB shares
    samba = {
      enable = true;
      settings = {
        global = {
          security = "user";
          "client min protocol" = "SMB2";
          "client max protocol" = "SMB3";
          "workgroup" = "WORKGROUP";
        };
      };
    };

    # libinput.enable = true;

    # Enable the OpenSSH daemon.
    openssh = {
      enable = true;

      settings = {
        KbdInteractiveAuthentication = false;
        PasswordAuthentication = false;
      };
    };

    # pipewire = {
    #   enable = true;
    #   alsa.enable = true;
    #   alsa.support32Bit = true;
    #   pulse.enable = true;
    # };

    # printing.enable = true;
    # pulseaudio.enable = false;
    tailscale.enable = true;

    # upower.enable = true;

    xserver = {
      enable = true;

      xkb = {
        layout = "us";
        variant = "";
      };
    };
  };

  system.stateVersion = "25.05";

  time.timeZone = "America/Detroit";

  users.users.duck = {
    isNormalUser = true;
    description = "Duck Nebuchadnezzar";
    extraGroups = [ "networkmanager" "wheel" "samba" ];
    packages = [ ];
  };

  # Add network file sharing packages
  environment.systemPackages = with pkgs; [ gvfs nfs-utils cifs-utils samba ];
}
