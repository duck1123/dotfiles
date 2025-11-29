{ ... }:
let
  hostname = "nasnix";
  nas-ip = "192.168.0.124";
  system = "x86_64-linux";
in {
  flake.modules = {
    generic.${hostname} = { config, ... }: {
      hosts.${hostname} = {
        inherit hostname system;
        id = "WUCVTEF-D2NOIGW-IFJPFKD-RHT7NSP-CZSIWM7-KLCHS3S-EIO3WFD-6DGAVAN";
        identity = config.identities.duck;
        name = hostname;

        features = {
          backups.enable = false;
          battery.enable = false;
          bitcoin.enable = true;
          bluetooth.enable = false;
          chm.enable = false;
          clojure.enable = true;
          common.enable = true;
          dbt.enable = false;
          dconf.enable = false;
          developer.enable = true;
          docker.enable = true;
          dunst.enable = false;
          emacs.enable = true;
          emacs-prelude.enable = false;
          email.enable = false;
          font.enable = true;
          gaming.enable = false;
          git.enable = true;
          gnome.enable = false;
          hyprland.enable = true;
          hyprpanel.enable = true;
          i3.enable = false;
          java.enable = false;
          jujutsu.enable = true;

          kubernetes = {
            client.enable = true;
            server.enable = true;
          };

          media = {
            enable = true;
            server.enable = true;
          };

          music.enable = false;
          ncmpcpp.enable = false;
          network.enable = true;
          nfs.enable = false;
          nix.enable = true;
          nostr.enable = true;
          nushell.enable = true;
          office.enable = false;
          pictures.enable = false;
          radio.enable = false;
          sddm.enable = true;
          sound.enable = false;
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
          plasma6.enable = false;
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

    nixos.${hostname} = { config, inputs, lib, modulesPath, pkgs, ... }:
      let
        core-module = {
          boot.loader.grub = {
            enable = true;
            device = "/dev/vda";
            useOSProber = true;
          };

          host = config.hosts.${hostname};
          system.stateVersion = "25.05";
          time.timeZone = "America/Detroit";
        };
        hardware-configuration = {
          imports = [ (modulesPath + "/profiles/qemu-guest.nix") ];

          boot = {
            extraModulePackages = [ ];

            initrd = {
              availableKernelModules =
                [ "ata_piix" "uhci_hcd" "virtio_pci" "sr_mod" "virtio_blk" ];
              kernelModules = [ ];
            };

            kernelModules = [ "kvm-intel" ];
          };

          fileSystems = {
            "/" = {
              device = "/dev/disk/by-uuid/1f61c6bf-be89-48fe-8081-7a74ec707a38";
              fsType = "ext4";
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
                [ "nfsvers=3" "ro" "hard" "timeo=600" "retrans=2" "_netdev" ];
            };

            "/mnt/videos" = {
              device = "${nas-ip}:/volume1/Videos";
              fsType = "nfs";
              options =
                [ "nfsvers=3" "rw" "hard" "timeo=600" "retrans=2" "_netdev" ];
            };
          };

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
        imports = specialisations.budgie.configuration.imports;
        specialisation = {
          # inherit (specialisations) budgie;
          # inherit (specialisations) gnome;
          # inherit (specialisations) hyprland;
          # inherit (specialisations) plasma6;
        };
      };
  };
}
