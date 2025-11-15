{ ... }:
let
  hostname = "nasnix";
  loadHosts = config: import ../../hosts/default.nix { inherit config; };
  nas-ip = "192.168.0.124";
in {
  flake.modules = {
    homeManager.${hostname} = { pkgs, config, ... }:
      let
        hosts = loadHosts config;
        host = hosts.${hostname};
      in {
        inherit host hosts;

        home = {
          packages = with pkgs; [ nerdfetch ];
          sessionPath = [ "$HOME/.cargo/bin:$PATH" "$HOME/.local/bin:$PATH" ];
        };
      };

    nixos.${hostname} = { config, inputs, lib, modulesPath, pkgs, ... }:
      let
        hosts = loadHosts config;
        host = hosts.${hostname};
        core-module = {
          inherit host hosts;

          boot.loader.grub = {
            enable = true;
            device = "/dev/vda";
            useOSProber = true;
          };

          environment.systemPackages = with pkgs; [ samba ];

          services.samba = {
            enable = true;
            settings.global = {
              security = "user";
              "client min protocol" = "SMB2";
              "client max protocol" = "SMB3";
              workgroup = "WORKGROUP";
            };
          };

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
