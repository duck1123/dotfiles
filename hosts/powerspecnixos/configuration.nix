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
    gnome.adwaita-icon-theme
    gnomeExtensions.appindicator
    git
    zsh
  ];

  hardware.pulseaudio.enable = false;

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
        # barrier port
        24800
        # Plex Media Server
        32400
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

  nix.settings.experimental-features = [ "nix-command" "flakes" ];

  # Allow unfree packages
  nixpkgs.config.allowUnfree = true;

  programs = {
    dconf.enable = true;

    gnupg.agent = {
      enable = true;
      enableSSHSupport = true;
    };

    steam.enable = true;

    zsh.enable = true;
  };

  security.rtkit.enable = true;

  services = {
    # emacs.enable = true;

    # desktopManager.plasma6.enable = true;

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

      pulse.enable = true;
    };

    plex.enable = true;

    # Enable CUPS to print documents.
    printing.enable = true;

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
          "Pixel" = {
            id =
              "R7RANTI-7SWMPTI-GDRAGOV-TK42PP3-PL3FHI5-LHGGVN3-PVNRIYO-FX7TAQM";
            autoAcceptFolders = true;
          };
        };

        folders = {
          "keepass" = {
            label = "keepass";
            path = "/home/duck/keepass";
            devices = [ "Pixel" ];
          };

          "org-roam" = {
            path = "/home/duck/org-roam";
            devices = [ "Pixel" ];
          };
        };

        options = { urAccepted = -1; };
      };
    };

    tailscale.enable = true;

    udev.packages = with pkgs; [ gnome.gnome-settings-daemon ];

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

        gdm.enable = true;
        # lightdm.enable = true;
        # sddm.enable = true;
      };

      # windowManager = {
      #   i3 = {
      #     enable = true;
      #     # package = pkgs.i3-gaps;
      #     # extraPackages = with pkgs; [ i3status i3lock polybar ];
      #   };
      # };

      # Enable touchpad support (enabled default in most desktopManager).
      # libinput.enable = true;

      xkb = {
        layout = "us";
        variant = "";
      };
    };
  };

  # Enable sound with pipewire.
  sound.enable = true;

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
    extraGroups = [ "docker" "libvirtd" "networkmanager" "wheel" ];
    packages = with pkgs; [ appimage-run emacs firefox ];
  };

  virtualisation = {
    docker.enable = true;
    libvirtd.enable = true;
  };
}
