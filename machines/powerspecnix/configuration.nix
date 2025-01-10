# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }: {
  imports = [ ./hardware-configuration.nix ];

  # Bootloader.
  boot = {
    binfmt.registrations.appimage = {
      interpreter = "${pkgs.appimage-run}/bin/appimage-run";
      magicOrExtension = "\\x7fELF....AI\\x02";
      mask = "\\xff\\xff\\xff\\xff\\x00\\x00\\x00\\x00\\xff\\xff\\xff";
      offset = 0;
      recognitionType = "magic";
      wrapInterpreterInShell = false;
    };

    loader = {
      efi.canTouchEfiVariables = true;
      systemd-boot.enable = true;
    };
  };

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
    adwaita-icon-theme
    gnomeExtensions.appindicator
    git
    k3s
    zsh
  ];

  hardware = {
    rtl-sdr.enable = true;
  };

  i18n = {
    # Select internationalisation properties.
    defaultLocale = "en_US.UTF-8";

    extraLocaleSettings = {
      LC_ADDRESS = "en_US.UTF-8";
      LC_IDENTIFICATION = "en_US.UTF-8";
      LC_MEASUREMENT = "en_US.UTF-8";
      LC_MONETARY = "en_US.UTF-8";
      LC_NAME = "en_US.UTF-8";
      LC_NUMERIC = "en_US.UTF-8";
      LC_PAPER = "en_US.UTF-8";
      LC_TELEPHONE = "en_US.UTF-8";
      LC_TIME = "en_US.UTF-8";
    };
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

      # allowedUDPPortRanges = [
      #   { from = 24800; to = 24800; }
      # ];
    };

    hostName = "powerspecnix"; # Define your hostname.

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

      libraries = with pkgs;
        [
          # Add any missing dynamic libraries for unpackaged programs
          # here, NOT in environment.systemPackages
          alsa-lib
          libGL
          renpy
        ];
    };

    steam.enable = true;

    zsh.enable = true;
  };

  security.rtkit.enable = true;

  services = {
    # emacs.enable = true;

    # desktopManager.plasma6.enable = true;

    displayManager = {
      defaultSession = "gnome";

      sddm = {
        enable = true;
        theme = "ocean";
        wayland.enable = true;
      };
    };

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

    syncthing = {
      enable = true;
      user = "duck";
      dataDir = "/home/duck/Documents";
      configDir = "/home/duck/Documents/.config/syncthing";
      openDefaultPorts = true;
      overrideFolders = true;
      overrideDevices = true;

      settings = {
        devices = {
          "Pixel 8" = {
            id =
              "7Y3NTUQ-MRUHGO4-5L34ZC7-EDRXHKA-QVCG7AJ-HWHIINY-OV5B2T7-OFQS2QP";
            autoAcceptFolders = true;
          };

          "steamdeck" = {
            id =
              "ZPO3QWJ-LQHVWBH-TAI3LLD-ZS6WSBM-N5IQ7JX-P4HUVF3-XNOX6N4-NBIF3AX";
            autoAcceptFolders = true;
          };

          "VallenPC" = {
            id =
              "TEED77K-QOLTQ37-BL76MFB-LJD46CW-EJ7CZTJ-7GQNEF6-FZAMQRP-BCCRTQ6";
            autoAcceptFolders = true;
          };
        };

        folders = {
          "Camera" = {
            label = "Camera";
            path = "/home/duck/Camera";
            devices = [ "Pixel 8" ];
          };

          "keepass" = {
            label = "keepass";
            path = "/home/duck/keepass";
            devices = [ "Pixel 8" "VallenPC" ];
          };

          "org-roam" = {
            path = "/home/duck/org-roam";
            devices = [ "Pixel 8" ];
            versioning = {
              type = "simple";
              params.keep = "10";
            };
          };

          "steamdeck-renpy" = {
            path = "/home/duck/.renpy";
            devices = [ "steamdeck" ];
          };
        };

        options = { urAccepted = -1; };
      };
    };

    tailscale.enable = true;

    udev.packages = with pkgs; [ gnome-settings-daemon ];

    # Configure keymap in X11
    xserver = {
      # Enable the X11 windowing system.
      enable = true;

      desktopManager = {
        # enlightenment.enable = true;

        # Enable the GNOME Desktop Environment.
        gnome.enable = true;

        # phosh.enable = true;

        # plasma5.enable = true;
        # plasma6.enable = true;
        # xfce.enable = true;
        # xterm.enable = false;
      };

      displayManager = {
        # defalutSession = "gnome";
        # defalutSession = "none+i3";

        # gdm.enable = true;
        # lightdm.enable = true;
      };

      # monitorConfig = ''
      #   xrandr --output HDMI-1 --left-of DP-3
      # '';

      windowManager = {
        i3 = {
          enable = true;
          package = pkgs.i3-gaps;
          # extraPackages = with pkgs; [ i3status i3lock polybar ];
        };
      };

      # Enable touchpad support (enabled default in most desktopManager).
      # libinput.enable = true;

      xkb = {
        layout = "us";
        variant = "";
      };
    };
  };

  # Enable sound with pipewire.
  # sound.enable = true;

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "23.11"; # Did you read the comment?

  # Set your time zone.
  time.timeZone = "America/Detroit";

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.duck = {
    isNormalUser = true;
    description = "Duck Nebuchadnezzar";
    extraGroups = [
      "dialout"
      "docker"
      "jackaudio"
      "libvirtd"
      "networkmanager"
      "plugdev"
      "realtime"
      "wheel"
    ];
    packages = with pkgs; [ appimage-run emacs firefox ];
    shell = pkgs.zsh;
  };

  virtualisation = {
    docker.enable = true;
    libvirtd.enable = true;
  };
}
