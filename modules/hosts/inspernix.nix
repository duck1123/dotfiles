{ ... }:
let
  hostname = "inspernix";
  mount-nas = true;
  nas-ip = "192.168.0.124";
  system = "x86_64-linux";
in
{
  flake.modules = {
    generic.${hostname} =
      { config, ... }:
      {
        hosts.${hostname} = {
          inherit hostname system;
          id = "OWMQLRL-CD5VB7H-A3T436E-6XT4H66-6XRF22Y-MQXMNAU-DFRNGOV-ADSKFAV";
          identity = config.identities.duck;
          name = hostname;

          features = {
            battery.enable = true;
            bluetooth.enable = true;
            clojure.enable = true;
            common.enable = true;
            docker.enable = true;
            emacs.enable = true;
            font.enable = true;
            gaming.enable = true;
            git.enable = true;
            gnome.enable = true;
            hyprland.enable = true;
            hyprpanel.enable = true;
            java.enable = true;
            jujutsu.enable = true;
            kubernetes.client.enable = true;
            network.enable = true;
            nfs.enable = true;
            nix.enable = true;
            nostr.enable = true;
            nushell.enable = true;
            sddm.enable = true;
            sound.enable = true;
            ssh.enable = true;
            starship.enable = true;
            stylix.enable = true;

            syncthing = {
              enable = true;

              shares = {
                camera.enable = false;
                keepass.enable = true;
                org-roam.enable = true;
                renpy.enable = true;
              };
            };

            tailscale.enable = true;
            touch.enable = true;
            vpn.enable = true;
            vscode.enable = true;
            xserver.enable = true;
            zsh.enable = true;
          };

          nixos.enable = true;
          pubkey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIPOVof4NXp3oq5lRnBawW5n8FEeMZY7H55NtCOeO+hoL duck@inspernix";
        };
      };

    homeManager.${hostname} =
      { config, pkgs, ... }:
      {
        host = config.hosts.${hostname};

        home = {
          packages = with pkgs; [
            cheese
            discord
            nerdfetch
          ];
          sessionPath = [
            "$HOME/.cargo/bin:$PATH"
            "$HOME/.local/bin:$PATH"
          ];
        };
      };

    nixos.${hostname} =
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
          host = config.hosts.${hostname};

          boot.loader = {
            systemd-boot.enable = true;
            efi.canTouchEfiVariables = true;
          };

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
              ];
            };
          };

          services = {
            gnome.gnome-keyring.enable = true;
            printing.enable = true;
          };

          time.timeZone = "America/Detroit";
        };
        hardware-configuration = {
          imports = [ (modulesPath + "/installer/scan/not-detected.nix") ];

          boot.initrd.availableKernelModules = [
            "nvme"
            "xhci_pci"
            "ahci"
            "usb_storage"
            "sd_mod"
          ];
          boot.initrd.kernelModules = [ ];
          boot.kernelModules = [ "kvm-amd" ];
          boot.extraModulePackages = [ ];

          fileSystems = {
            "/" = {
              device = "/dev/disk/by-uuid/b0dd8d1b-b9e2-4ca8-87b2-d99d40809cfd";
              fsType = "ext4";
            };

            "/boot" = {
              device = "/dev/disk/by-uuid/1D60-65FB";
              fsType = "vfat";
              options = [
                "fmask=0077"
                "dmask=0077"
              ];
            };
          }
          // lib.optionalAttrs mount-nas {
            "/mnt/audiobooks" = {
              device = "${nas-ip}:/volume1/Audiobooks";
              fsType = "nfs";
              options = [
                "nfsvers=3"
                "rw"
                "hard"
                "timeo=600"
                "retrans=2"
                "_netdev"
              ];
            };

            "/mnt/books" = {
              device = "${nas-ip}:/volume1/Books";
              fsType = "nfs";
              options = [
                "nfsvers=3"
                "rw"
                "hard"
                "timeo=600"
                "retrans=2"
                "_netdev"
              ];
            };

            "/mnt/downloads" = {
              device = "${nas-ip}:/volume1/Downloads";
              fsType = "nfs";
              options = [
                "nfsvers=3"
                "rw"
                "hard"
                "timeo=600"
                "retrans=2"
                "_netdev"
              ];
            };

            "/mnt/movies" = {
              device = "${nas-ip}:/volume1/Movies";
              fsType = "nfs";
              options = [
                "nfsvers=3"
                "rw"
                "hard"
                "timeo=600"
                "retrans=2"
                "_netdev"
              ];
            };

            "/mnt/music" = {
              device = "${nas-ip}:/volume1/Music";
              fsType = "nfs";
              options = [
                "nfsvers=3"
                "rw"
                "hard"
                "timeo=600"
                "retrans=2"
                "_netdev"
              ];
            };

            "/mnt/photos" = {
              device = "${nas-ip}:/volume1/Photos";
              fsType = "nfs";
              options = [
                "nfsvers=3"
                "rw"
                "hard"
                "timeo=600"
                "retrans=2"
                "_netdev"
              ];
            };

            "/mnt/roms" = {
              device = "${nas-ip}:/volume1/Roms";
              fsType = "nfs";
              options = [
                "nfsvers=3"
                "rw"
                "hard"
                "timeo=600"
                "retrans=2"
                "_netdev"
              ];
            };

            "/mnt/tv" = {
              device = "${nas-ip}:/volume1/TV";
              fsType = "nfs";
              options = [
                "nfsvers=3"
                "rw"
                "hard"
                "timeo=600"
                "retrans=2"
                "_netdev"
              ];
            };

            "/mnt/videos" = {
              device = "${nas-ip}:/volume1/Videos";
              fsType = "nfs";
              options = [
                "nfsvers=3"
                "rw"
                "hard"
                "timeo=600"
                "retrans=2"
                "_netdev"
              ];
            };
          };

          swapDevices = [
            {
              device = "/dev/disk/by-uuid/34a6b6d6-cccf-474e-a2d6-7b3e9dc29d80";
            }
          ];

          networking.useDHCP = lib.mkDefault true;
          nixpkgs.hostPlatform = lib.mkDefault "x86_64-linux";
          hardware.cpu.amd.updateMicrocode = lib.mkDefault config.hardware.enableRedistributableFirmware;
        };
        core = [
          core-module
          hardware-configuration
          inputs.self.modules.nixos.base
        ];
        mkSpecialisation = module: {
          inheritParentConfig = false;
          configuration = {
            imports = core ++ [ module ];
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
      in
      {
        _module.args = { inherit inputs; };
        imports = specialisations.hyprland.configuration.imports  ++ [ inputs.self.modules.nixos.nixos ];
        specialisation = {
          inherit (specialisations) budgie;
          inherit (specialisations) gnome;
          # inherit (specialisations) hyprland;
          # inherit (specialisations) i3;
          # inherit (specialisations) plasma6;
        };
      };
  };
}
