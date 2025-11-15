{ ... }:
let
  hostname = "powerspecnix";
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
          packages = with pkgs; [
            alacritty
            colmena
            discord
            distrobox
            docker
            # fastfetch
            ffmpeg
            # gitu
            # kakoune
            # kb
            # keet
            # khoj
            kty
            libnotify
            # logseq
            # mdcat
            minio-client
            # mullvad-browser
            networkmanager
            nix-tree
            # obsidian
            # onlyoffice-bin
            playerctl
            # postman
            # sparrow
            syncthing
            telegram-desktop
            # tilt
            transmission_4-gtk
            # tree
            unzip
            # virtualbox
            vscode
            wine
            xsel
            # yq
          ];

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

          environment.systemPackages = with pkgs; [ git ];

          hardware = {
            flipperzero.enable = true;
            rtl-sdr.enable = true;
          };

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
            plex.enable = true;
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
          # inherit (specialisations) i3;
          # inherit (specialisations) hyprland;
          # inherit (specialisations) plasma6;
        };
      };
  };
}
