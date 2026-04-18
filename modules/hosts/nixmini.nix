{ ... }:
let
  hostname = "nixmini";
  system = "x86_64-linux";
in
{
  flake.modules = {
    generic.${hostname} =
      { config, ... }:
      {
        hosts.${hostname} = {
          inherit hostname system;
          id = "";
          identity = config.identities.duck;
          name = hostname;

          features = {
            bluetooth.enable = true;
            clojure.enable = true;
            common.enable = true;
            docker.enable = true;
            emacs.enable = true;
            font.enable = true;
            git.enable = true;
            hyprland.enable = true;
            hyprpanel.enable = false;

            kubernetes = {
              client.enable = true;
              gpu = "amd";
              server.enable = true;
              serverAddr = "https://nasnix:6443";
              tokenFile = ./../../secrets/k3s-token.yaml;
            };

            media = {
              enable = false;
              server.enable = false;
            };

            network.enable = true;
            nix.enable = true;
            nushell.enable = true;
            sddm.enable = true;
            sound.enable = true;
            sleep.enable = true;
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

          pubkey = "";
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
        };

        hardware-configuration = {
          imports = [ (modulesPath + "/installer/scan/not-detected.nix") ];

          boot = {
            extraModulePackages = [ ];

            initrd = {
              availableKernelModules = [
                "xhci_pci"
                "ahci"
                "nvme"
                "usbhid"
                "usb_storage"
                "sd_mod"
              ];
              kernelModules = [ ];
            };

            kernelModules = [ ];
          };

          fileSystems = {
            "/" = {
              device = "/dev/disk/by-uuid/cd3247f5-ff09-4d73-95e3-530d69ea72a3";
              fsType = "ext4";
            };

            "/boot" = {
              device = "/dev/disk/by-uuid/CB7E-85E8";
              fsType = "vfat";
              options = [
                "fmask=0077"
                "dmask=0077"
              ];
            };
          };

          hardware.cpu.intel.updateMicrocode = lib.mkDefault config.hardware.enableRedistributableFirmware;

          swapDevices = [ ];
          networking.useDHCP = lib.mkDefault true;
          nixpkgs.hostPlatform = lib.mkDefault "x86_64-linux";
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
          plasma6 = mkSpecialisation environments-plasma6;
        };
      in
      {
        _module.args = { inherit inputs; };
        imports = specialisations.plasma6.configuration.imports;
        specialisation = {
          inherit (specialisations) budgie;
        };
      };
  };
}
