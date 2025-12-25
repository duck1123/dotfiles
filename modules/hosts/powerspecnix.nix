{ ... }:
let
  hostname = "powerspecnix";
  mount-nas = true;
  nas-ip = "192.168.0.124";
  system = "x86_64-linux";
in {
  flake.modules = {
    generic.${hostname} = { config, ... }:
      let identity = config.identities.duck;
      in {
        hosts.${hostname} = {
          inherit hostname identity system;

          features = {
            backups.enable = true;
            battery.enable = false;
            bitcoin.enable = false;
            bluetooth.enable = true;
            chm.enable = false;
            clojure.enable = true;
            common.enable = true;
            chat.enable = true;
            dbt.enable = false;
            dconf.enable = false;
            developer.enable = true;
            docker.enable = true;
            dunst.enable = false;
            emacs.enable = true;
            emacs-prelude.enable = false;
            email.enable = true;
            flipper.enable = true;
            font.enable = true;
            gaming.enable = true;
            git.enable = true;
            gnome.enable = true;
            hyprland.enable = true;
            hyprpanel.enable = true;
            i3.enable = false;
            java.enable = true;
            jujutsu.enable = true;

            kubernetes = {
              client.enable = true;
              server.enable = false;
            };

            media = {
              enable = true;
              server.enable = false;
            };

            music.enable = false;
            ncmpcpp.enable = false;
            network.enable = true;
            nfs.enable = false;
            nix.enable = true;
            nostr.enable = true;
            nushell.enable = true;
            office.enable = true;
            pictures.enable = true;
            radio.enable = false;
            sddm.enable = true;
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
            vpn.enable = true;
            vscode.enable = true;
            waybar.enable = false;
            xserver.enable = true;
            zsh.enable = true;
          };

          id =
            "UFCCQLJ-3EKBVCQ-O5CNVM5-ERJQAQG-JWKQRPU-7FOZHPG-VMEOMKJ-KZSUFQK";
          name = hostname;
          nixos.enable = true;
          pubkey =
            "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIHaYYrgkSRYftmy5p0TbtGBWTR+oJmP6hkB8eoWFB7va ${identity.username}@${hostname}";
        };
      };

    homeManager.${hostname} = { config, ... }: {
      host = config.hosts.${hostname};
    };

    nixos.${hostname} = { config, inputs, lib, modulesPath, pkgs, ... }:
      let
        core-module = {
          host = config.hosts.${hostname};

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

            nix-ld = {
              enable = true;
              libraries = with pkgs; [ alsa-lib libGL ];
            };
          };

          services = {
            gnome.gnome-keyring.enable = true;
            flatpak.enable = true;
            printing.enable = true;
            udev.packages = with pkgs; [ gnome-settings-daemon ];
          };

          system.stateVersion = "25.05";

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
              availableKernelModules =
                [ "xhci_pci" "ahci" "usbhid" "usb_storage" "sd_mod" ];
              kernelModules = [ ];
            };

            kernelModules = [ "kvm-amd" ];
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
          } // lib.optionalAttrs mount-nas {
            "/mnt/audiobooks" = {
              device = "${nas-ip}:/volume1/Audiobooks";
              fsType = "nfs";
              options =
                [ "nfsvers=3" "rw" "hard" "timeo=600" "retrans=2" "_netdev" ];
            };

            "/mnt/books" = {
              device = "${nas-ip}:/volume1/Books";
              fsType = "nfs";
              options =
                [ "nfsvers=3" "rw" "hard" "timeo=600" "retrans=2" "_netdev" ];
            };

            "/mnt/downloads" = {
              device = "${nas-ip}:/volume1/Downloads";
              fsType = "nfs";
              options =
                [ "nfsvers=3" "rw" "hard" "timeo=600" "retrans=2" "_netdev" ];
            };

            "/mnt/music" = {
              device = "${nas-ip}:/volume1/Music";
              fsType = "nfs";
              options =
                [ "nfsvers=3" "rw" "hard" "timeo=600" "retrans=2" "_netdev" ];
            };

            "/mnt/photos" = {
              device = "${nas-ip}:/volume1/Photos";
              fsType = "nfs";
              options =
                [ "nfsvers=3" "rw" "hard" "timeo=600" "retrans=2" "_netdev" ];
            };

            "/mnt/roms" = {
              device = "${nas-ip}:/volume1/Roms";
              fsType = "nfs";
              options =
                [ "nfsvers=3" "rw" "hard" "timeo=600" "retrans=2" "_netdev" ];
            };

            "/mnt/videos" = {
              device = "${nas-ip}:/volume1/Videos";
              fsType = "nfs";
              options =
                [ "nfsvers=3" "rw" "hard" "timeo=600" "retrans=2" "_netdev" ];
            };
          };

          hardware.cpu.amd.updateMicrocode =
            lib.mkDefault config.hardware.enableRedistributableFirmware;

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
          };

          nixpkgs.hostPlatform = lib.mkDefault "x86_64-linux";

          swapDevices = [ ];
        };
        core =
          [ core-module hardware-configuration inputs.self.modules.nixos.base ];
        mkSpecialisation = env-module: {
          inheritParentConfig = false;
          configuration = {
            imports = core ++ [ env-module ];
            _module.args = { inherit inputs; };
          };
        };
        specialisations = with inputs.self.modules.nixos; {
          budgie = mkSpecialisation environments-budgie;
          hyprland = mkSpecialisation environments-hyprland;
          gnome = mkSpecialisation environments-gnome;
          i3 = mkSpecialisation environments-i3;
          plasma6 = mkSpecialisation environments-plasma6;
        };
      in {
        _module.args = { inherit inputs; };
        imports = specialisations.hyprland.configuration.imports;
        specialisation = {
          inherit (specialisations) budgie;
          inherit (specialisations) gnome;
          # inherit (specialisations) i3;
          # inherit (specialisations) hyprland;
          inherit (specialisations) plasma6;
        };
      };
  };
}
