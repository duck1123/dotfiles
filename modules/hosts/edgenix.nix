_:
let
  hostname = "edgenix";
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
          id = "PVIXR2M-YL4TSHJ-7PHLJOO-3EYFEJR-TVT5VS6-EFRNFGP-7QGVFJW-25AAVAF";
          identity = config.identities.duck;
          name = hostname;

          environments = {
            primary = "plasma6";
          };

          features = {
            bluetooth.enable = true;
            clojure.enable = false;
            common.enable = true;
            docker.enable = true;
            emacs.enable = true;
            firefox.enable = true;
            font.enable = true;
            git.enable = true;
            glances.enable = true;

            kubernetes = {
              client.enable = true;
              gpu = "amd";
              server.enable = true;
              serverAddr = "https://nasnix:6443";
              tokenFile = ./../../secrets/k3s-token.yaml;
            };

            media = {
              enable = true;
              server.enable = true;
            };

            network.enable = true;
            nix = {
              atticPush.enable = true;
              enable = true;
            };
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
                org-roam.enable = false;
                renpy.enable = false;
              };
            };

            tailscale.enable = true;
            vscode.enable = false;
            xserver.enable = true;
            zen-browser.enable = true;
            zsh.enable = true;
          };

          nixos.enable = true;

          pubkey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIIqJGVkP5pUAJrJyW7Gdqv5XO3ImDNjtYuNS2rEUj7bt duck@edgenix";
        };
      };

    homeManager.${hostname} =
      { config, pkgs, ... }:
      {
        host = config.hosts.${hostname};

        home = {
          packages = with pkgs; [
            guake
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
        ...
      }:
      let
        core-module = {
          boot.loader = {
            systemd-boot.enable = true;
            efi.canTouchEfiVariables = true;
          };

          host = config.hosts.${hostname};
          time.timeZone = "America/Detroit";

          # Pin the LAN IP so it doesn't drift under DHCP -- other hosts/services
          # (k3s serverAddr, ssh known_hosts, homepage widgets) reference it by IP.
          networking.networkmanager.ensureProfiles.profiles."Wired connection 1" = {
            connection = {
              id = "Wired connection 1";
              type = "ethernet";
              interface-name = "eno1";
            };
            ipv4 = {
              method = "manual";
              addresses = "192.168.0.22/24";
              gateway = "192.168.0.1";
              dns = "192.168.0.1;";
            };
          };
        };

        hardware-configuration = {
          imports = [ (modulesPath + "/installer/scan/not-detected.nix") ];

          boot = {
            extraModulePackages = [ ];

            initrd = {
              availableKernelModules = [
                "xhci_pci"
                "ehci_pci"
                "ahci"
                "usbhid"
                "usb_storage"
                "sd_mod"
                "sr_mod"
              ];
              kernelModules = [ ];
            };

            kernelModules = [ ];
          };

          fileSystems = {
            "/" = {
              device = "/dev/disk/by-uuid/16510971-9a21-482a-ad63-1cff4f669212";
              fsType = "ext4";
            };

            "/boot" = {
              device = "/dev/disk/by-uuid/3453-AB06";
              fsType = "vfat";
              options = [
                "fmask=0077"
                "dmask=0077"
              ];
            };
          }
          // inputs.self.lib.nas-mounts {
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
              "slskd_downloads"
              "TV"
              "Videos"
            ];
          };

          hardware.cpu.intel.updateMicrocode = lib.mkDefault config.hardware.enableRedistributableFirmware;

          swapDevices = [ ];
          networking.useDHCP = lib.mkDefault true;
          nixpkgs.hostPlatform = lib.mkDefault "x86_64-linux";
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
  };
}
