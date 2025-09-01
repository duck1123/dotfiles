{ pkgs, ... }: {
  hardware.bluetooth.enable = true;

  programs = {
    dconf.enable = true;
    firefox.enable = true;

    gnupg.agent = {
      enable = true;
      enableSSHSupport = true;
    };

    nix-ld = {
      enable = true;
      libraries = with pkgs; [
        alsa-lib
        libGL
        # renpy
      ];
    };

    zsh.enable = true;
  };

  services = {
    gnome.gnome-keyring.enable = true;

    # Enable the OpenSSH daemon.
    openssh = {
      enable = true;

      settings = {
        KbdInteractiveAuthentication = false;
        PasswordAuthentication = false;
      };
    };

    printing.enable = true;
    tailscale.enable = true;

    upower.enable = true;

    xserver = {
      enable = true;

      xkb = {
        layout = "us";
        variant = "";
      };
    };
  };

  system.stateVersion = "24.11";

  time.timeZone = "America/Detroit";

  users.users.duck = {
    isNormalUser = true;
    description = "Duck Nebuchadnezzar";
    extraGroups = [ "networkmanager" "wheel" ];
    packages = [ ];
  };
}
