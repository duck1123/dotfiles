{ host, pkgs, ... }:
let inherit (host) hostname;
in {
  environment.systemPackages = with pkgs; [ git gvfs zsh ];

  hardware = {
    bluetooth.enable = true;
    flipperzero.enable = true;
    rtl-sdr.enable = true;
  };

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

    # Enable networking
    networkmanager.enable = true;
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

  security.rtkit.enable = true;

  services = {
    gnome.gnome-keyring.enable = true;

    gvfs.enable = true;

    flatpak.enable = true;

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

    plex.enable = true;

    # Enable CUPS to print documents.
    printing.enable = true;

    pulseaudio.enable = false;

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
