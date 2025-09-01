{ pkgs, ... }: {
  environment.systemPackages = with pkgs; [ git zsh ];

  hardware = {
    bluetooth.enable = true;
    flipperzero.enable = true;
    rtl-sdr.enable = true;
  };

  nixpkgs.config.chromium.enableWideVine = true;

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

    zsh.enable = true;
  };

  services = {
    gnome.gnome-keyring.enable = true;

    flatpak.enable = true;

    # Enable the OpenSSH daemon.
    openssh = {
      enable = true;

      settings = {
        KbdInteractiveAuthentication = false;
        PasswordAuthentication = false;
      };
    };

    plex.enable = true;

    # Enable CUPS to print documents.
    printing.enable = true;

    tailscale.enable = true;

    udev.packages = with pkgs; [ gnome-settings-daemon ];

    # Configure keymap in X11
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

  virtualisation = {
    docker.enable = true;
    libvirtd.enable = true;
  };

  xdg.portal = {
    enable = true;
    config.common.default = "*";
  };
}
