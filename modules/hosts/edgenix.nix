{ ... }:
let
  hostname = "edgenix";
  nas-ip = "192.168.0.124";
  system = "x86_64-linux";
in {
  flake.modules = {
    generic.${hostname} = { config, ... }: {
      hosts.${hostname} = {
        inherit hostname system;
        id = "C5K6RA6-WXFIGSZ-5PKIJY2-BCOGWYM-XTGSO4T-F4NQT6I-EOIUMT7-ZTHWHAF";
        identity = config.identities.duck;
        name = hostname;

        features = {
          bluetooth.enable = true;
          clojure.enable = true;
          common.enable = true;
          dconf.enable = false;
          developer.enable = false;
          docker.enable = true;
          dunst.enable = false;
          emacs.enable = true;
          emacs-prelude.enable = false;
          email.enable = false;
          flipper.enable = false;
          font.enable = true;
          gaming.enable = true;
          git.enable = true;
          gnome.enable = false;
          hyprland.enable = true;
          hyprpanel.enable = true;
          i3.enable = false;
          java.enable = false;
          jujutsu.enable = false;

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
          nostr.enable = false;
          nushell.enable = true;
          office.enable = false;
          pictures.enable = false;
          radio.enable = false;
          samba.enable = false;
          sddm.enable = true;
          sound.enable = true;
          ssh.enable = true;
          starship.enable = true;
          stylix.enable = true;

          syncthing = {
            enable = false;

            shares = {
              camera.enable = false;
              keepass.enable = true;
              org-roam.enable = false;
              renpy.enable = false;
            };
          };

          tailscale.enable = true;
          touch.enable = false;
          vim.enable = false;
          virtualization.enable = false;
          vscode.enable = true;
          waybar.enable = false;
          xserver.enable = true;
          zsh.enable = true;
        };

        nixos = {
          enable = true;
          budgie.enable = false;
          gnome.enable = false;
          hyprland.enable = false;
          i3.enable = false;
          plasma6.enable = true;
        };
      };
    };

    homeManager.${hostname} = { config, pkgs, ... }: {
      host = config.hosts.${hostname};

      home = {
        packages = with pkgs; [ nerdfetch ];
        sessionPath = [ "$HOME/.cargo/bin:$PATH" "$HOME/.local/bin:$PATH" ];
      };
    };

    nixos.${hostname} = { config, inputs, lib, modulesPath, ... }:
      let
        core-module = {
          boot.loader.grub = {
            enable = true;
            device = "/dev/sda";
            useOSProber = true;
          };

          host = config.hosts.${hostname};
          system.stateVersion = "25.05";
          time.timeZone = "America/Detroit";
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
          };

          fileSystems."/boot" = {
            device = "/dev/disk/by-uuid/3453-AB06";
            fsType = "vfat";
            options = [ "fmask=0077" "dmask=0077" ];
          };

          hardware.cpu.intel.updateMicrocode =
            lib.mkDefault config.hardware.enableRedistributableFirmware;

          swapDevices = [ ];
          networking.useDHCP = lib.mkDefault true;
          nixpkgs.hostPlatform = lib.mkDefault "x86_64-linux";
        };
        core =
          [ core-module hardware-configuration inputs.self.modules.nixos.base ];
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
          plasma6 = mkSpecialisation environments-plasma6;
        };
      in {
        _module.args = { inherit inputs; };
        imports = specialisations.plasma6.configuration.imports;
        specialisation = {
          inherit (specialisations) budgie;
          # inherit (specialisations) gnome;
          # inherit (specialisations) hyprland;
          # inherit (specialisations) plasma6;
        };
      };
  };
}
