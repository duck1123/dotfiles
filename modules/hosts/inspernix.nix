{ ... }:
let
  hostname = "inspernix";
  loadHosts = config: import ../../hosts/default.nix { inherit config; };
  mount-nas = false;
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
          packages = with pkgs; [ cheese discord nerdfetch ];
          sessionPath = [ "$HOME/.cargo/bin:$PATH" "$HOME/.local/bin:$PATH" ];
        };
      };

    nixos.${hostname} = { config, inputs, lib, modulesPath, pkgs, ... }:
      let
        hosts = loadHosts config;
        host = hosts.${hostname};
        core-module = {
          inherit host hosts;

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
              libraries = with pkgs; [ alsa-lib libGL ];
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

          boot.initrd.availableKernelModules =
            [ "nvme" "xhci_pci" "ahci" "usb_storage" "sd_mod" ];
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
              options = [ "fmask=0077" "dmask=0077" ];
            };
          } // lib.optionalAttrs mount-nas {
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

            "/mnt/videos" = {
              device = "${nas-ip}:/volume1/Videos";
              fsType = "nfs";
              options =
                [ "nfsvers=3" "rw" "hard" "timeo=600" "retrans=2" "_netdev" ];
            };
          };

          swapDevices = [{
            device = "/dev/disk/by-uuid/34a6b6d6-cccf-474e-a2d6-7b3e9dc29d80";
          }];

          networking.useDHCP = lib.mkDefault true;
          nixpkgs.hostPlatform = lib.mkDefault "x86_64-linux";
          hardware.cpu.amd.updateMicrocode =
            lib.mkDefault config.hardware.enableRedistributableFirmware;
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
          i3 = mkSpecialisation environments-i3;
          plasma6 = mkSpecialisation environments-plasma6;
        };
      in {
        _module.args = { inherit inputs; };
        imports = specialisations.hyprland.configuration.imports;
        specialisation = {
          inherit (specialisations) budgie;
          # inherit (specialisations) gnome;
          # inherit (specialisations) hyprland;
          # inherit (specialisations) i3;
          # inherit (specialisations) plasma6;
        };
      };
  };
}
