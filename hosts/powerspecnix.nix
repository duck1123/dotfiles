{ config, ... }:
let
  hostname = "powerspecnix";
  mount-nas = true;
  nas-ip = "192.168.0.124";
  identity = config.identities.duck;
in
{
  inherit identity;
  system = "x86_64-linux";

  environments = {
    primary = "niri";
    hyprland.enable = true;
    # niri.enable = true;
    plasma6.enable = true;
  };

  features = {
    backups.enable = true;
    battery.enable = false;
    bitcoin.enable = false;
    bluetooth.enable = true;
    clojure.enable = false;
    common.enable = true;
    chat.enable = false;
    dbt.enable = false;
    developer.enable = true;
    docker.enable = false;
    emacs.enable = true;
    email.enable = true;
    flipper.enable = false;
    firefox.enable = true;
    font.enable = true;
    gaming.enable = true;
    git.enable = true;
    glances.enable = true;
    hyprland = {
      monitor = [
        {
          output = "HDMI-A-1";
          mode = "1920x1080";
          position = "0x0";
          scale = 1;
        }
        {
          output = "DP-3";
          mode = "1920x1080";
          position = "1920x0";
          scale = 1;
        }
      ];
    };
    java.enable = true;
    jujutsu.enable = true;

    kubernetes = {
      client.enable = true;
      gpu = "amd";
      server.enable = false;
      serverAddr = "https://nasnix:6443";
      tokenFile = ./../secrets/k3s-token.yaml;
    };

    media = {
      enable = true;
      server.enable = false;
    };

    music.enable = true;
    network.enable = true;
    nfs.enable = false;
    nix = {
      atticPush.enable = true;
      enable = true;
    };
    nostr.enable = true;
    nushell.enable = true;
    obsidian.enable = true;
    pictures.enable = false;
    python.enable = true;
    radio.enable = false;
    sddm.enable = false;
    sound.enable = true;
    ssh.enable = true;
    starship.enable = true;
    stylix.enable = true;

    syncthing = {
      enable = true;

      shares = {
        camera.enable = true;
        keepass.enable = true;
        org-roam.enable = true;
        renpy.enable = true;
        sims4.enable = true;
      };
    };

    tailscale.enable = true;
    touch.enable = false;
    vim.enable = false;
    virtualization.enable = false;
    vr.enable = false;
    vpn.enable = true;
    vscode.enable = false;
    waybar.enable = false;
    waydroid.enable = true;
    windmill.enable = true;
    xserver.enable = true;
    zen-browser.enable = true;
    zsh.enable = true;
  };

  id = "UFCCQLJ-3EKBVCQ-O5CNVM5-ERJQAQG-JWKQRPU-7FOZHPG-VMEOMKJ-KZSUFQK";
  nixos.enable = true;
  pubkey = "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABgQC1MRZsSlV1woP3bD5T1W7BjN+buwU70mo1BmtZNFHZtOogDT5FH0jy9itMV+JwHyZCbG325RvD9eRrG1GPJsUHM7bsUNuKwYpTOR868CanKthDr5fp5pGVBxzUKZnl1YA/rWbaEO9M3iSVwxmOmlc0MMgzUujCfr3Cc2pZHEXvBXW6xasMEh7DA6zbPqrSMEk2/d2oVCDD3DBfJKzLX6oLfknLAMWPFcik6UBbAhm+xhc0jbjU1etOZc6Xau5aXWODo9xE2v/PhlSgmFLS1BeFQvfFNJkF7ADdtGBiI1eR76uxV3RHOJ45/vNw1Dvtf3vNSg4qK7xz7osSfKUvvnQjWipFllPxwzkpcK93Bz4JrrYwH9gwYi2roE6cEMl3HI7NIjTMDuUhjMeKNedn4FG0jQOvJRfHkBDnHq4vbMdJErRf1x07AMTicT7HoCJ2mKfrEmVUgekT1xyWN+THwQfFnaOj1sqSWYzydXmKo0VJhzvuS605JFO9lU8Fi3qmUVs= ${identity.username}@${hostname}";

  modules.homeManager =
    { pkgs, ... }:
    {
      home.packages = with pkgs; [
        claude-code
        qbittorrent
      ];
    };

  modules.nixos =
    {
      config,
      inputs,
      lib,
      modulesPath,
      pkgs,
      ...
    }:
    let
      core-module = {
        boot.loader = {
          systemd-boot.enable = true;
          efi.canTouchEfiVariables = true;
        };

        environment.systemPackages = with pkgs; [ git ];
        nixpkgs.config.chromium.enableWideVine = true;

        programs = {
          dconf.enable = true;

          gnupg.agent = {
            enable = true;
            enableSSHSupport = true;
          };
        };

        services = {
          gnome.gnome-keyring.enable = true;
          flatpak.enable = true;
          printing.enable = true;
          udev.packages = with pkgs; [ gnome-settings-daemon ];
        };

        networking.hosts = {
          "192.168.0.16" = [ "nasnix" ];
          "192.168.0.25" = [ "nixmini" ];
        };

        time.timeZone = "America/Detroit";

        virtualisation = {
          docker.enable = true;
          libvirtd.enable = true;
        };
      };

      hardware-configuration = {
        imports = [ (modulesPath + "/installer/scan/not-detected.nix") ];

        boot = {
          extraModulePackages = [ ];

          initrd = {
            availableKernelModules = [
              "xhci_pci"
              "ahci"
              "usbhid"
              "usb_storage"
              "sd_mod"
            ];
            kernelModules = [ ];
          };

          kernelModules = [
            "kvm-amd"
            "ntsync"
          ];
        };

        fileSystems = {
          "/" = {
            device = "/dev/disk/by-uuid/e8d6b0eb-28dd-424c-bffb-52eafaeec27f";
            fsType = "ext4";
          };

          "/boot" = {
            device = "/dev/disk/by-uuid/4D62-CC29";
            fsType = "vfat";
          };

          "/mnt/data3" = {
            device = "/dev/disk/by-uuid/0d256fc5-070b-43c3-a963-04a0ad2843bd";
            fsType = "ext4";
            options = [ "nofail" ];
          };
        }
        // lib.optionalAttrs mount-nas (
          inputs.self.lib.nas-mounts {
            ip = nas-ip;
            shares = [
              "Audiobooks"
              "Books"
              "Comedy"
              "Downloads"
              "Movies"
              "Music"
              "Photos"
              "Pinchflat"
              "Roms"
              "TV"
              "Videos"
            ];
          }
        );

        hardware.cpu.amd.updateMicrocode = lib.mkDefault config.hardware.enableRedistributableFirmware;

        networking = {
          interfaces = {
            # enp37s0.useDHCP = lib.mkDefault true;
            # wlp35s0.useDHCP = lib.mkDefault true;
          };

          # Enables DHCP on each ethernet and wireless interface. In case of scripted networking
          # (the default) this is the recommended approach. When using systemd-networkd it's
          # still possible to use this option, but it's recommended to use it in conjunction
          # with explicit per-interface declarations with `networking.interfaces.<interface>.useDHCP`.
          useDHCP = lib.mkDefault true;

          # Use Pi-hole for DNS regardless of what DHCP hands out.
          # Pi-hole provides split-horizon resolution for *.local and *.dev.kronkltd.net.
          # nameservers = [ "192.168.0.242" "8.8.8.8" "1.1.1.1" ];
        };

        nixpkgs.hostPlatform = lib.mkDefault "x86_64-linux";

        swapDevices = [ ];
      };
    in
    {
      _module.args = { inherit inputs; };
      imports = [
        core-module
        hardware-configuration
        inputs.self.modules.nixos.base
      ];
    };
}
