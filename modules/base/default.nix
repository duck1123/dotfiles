{ config, inputs, pkgs, ... }:
let
  username = config.username;
  hostname = config.hostname;
in {
  environment.systemPackages = with pkgs; [
    # adwaita-icon-theme
    # gnomeExtensions.appindicator
    git
    k3s
    zsh
  ];

  hardware = {
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

        ## k3s
        6443
        # 2379
        # 2380
      ];

      allowedUDPPorts = [
        ## k3s
        8472
      ];
    };

    hostName = "${hostname}"; # Define your hostname.

    # Enable networking
    networkmanager.enable = true;

    # Enables wireless support via wpa_supplicant.
    # wireless.enable = true;
  };

  nix = {
    gc = {
      automatic = true;
      options = "--delete-older-than 14d";
    };

    optimise.automatic = true;

    settings = {
      auto-optimise-store = true;
      experimental-features = [ "nix-command" "flakes" ];
    };
  };

  # Allow unfree packages
  nixpkgs.config = {
    allowUnfree = true;
    chromium.enableWideVine = true;
  };

  programs = {
    dconf.enable = true;

    gnupg.agent = {
      enable = true;
      enableSSHSupport = true;
    };

    nix-ld = {
      enable = true;

      libraries = with pkgs; [ alsa-lib libGL renpy ];
    };

    steam.enable = true;

    zsh.enable = true;
  };

  security.rtkit.enable = true;

  services = {
    gnome.gnome-keyring.enable = true;

    flatpak.enable = true;

    k3s = {
      # enable = true;
      enable = false;
      role = "server";
      extraFlags = toString [ "--disable=traefik" ];
    };

    nfs.server = {
      enable = false;
      # Fixed ports for firewall.
      statdPort = 4000;
      lockdPort = 4001;
      mountdPort = 4002;

      exports = ''
        /mnt/nfs    *(ro,insecure,all_squash)
      '';
    };

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
