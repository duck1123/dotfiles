{...}: {
  hardware = {
    bluetooth.enable = true;
    flipperzero.enable = true;
    rtl-sdr.enable = true;
  };

  networking = {
    # Enable networking
    networkmanager.enable = true;

  };
  
  programs = {
    dconf.enable = true;

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

    steam.enable = true;
    zsh.enable = true;

  };

  security.rtkit.enable = true;

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

    pipewire = {
      enable = true;

      alsa = {
        enable = true;
        support32Bit = true;
      };

      jack.enable = true;
      pulse.enable = true;
    };

    printing.enable = true;
    pulseaudio.enable = false;
    tailscale.enable = true;

    xserver = {
      enable = true;

      xkb = {
        layout = "us";
        variant = "";
      };
    };
  };

  system.stateVersion = "25.05";

  # Set your time zone.
  time.timeZone = "America/Detroit";


  
}
