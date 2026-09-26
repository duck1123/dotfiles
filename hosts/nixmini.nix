{ config, ... }:
{
  system = "x86_64-linux";
  id = "745DPT4-HGNIGCP-O4FV22H-SZ3ALWS-YV2EJNX-HMMIUPP-YWOVJHZ-FT3DXAM";
  identity = config.identities.duck;

  environments = {
    primary = "plasma6";
  };

  features = {
    bluetooth.enable = true;
    clojure.enable = true;
    common.enable = true;
    docker.enable = true;
    emacs.enable = true;
    firefox.enable = true;
    font.enable = true;
    gaming.enable = true;
    git.enable = true;
    glances.enable = true;

    kubernetes = {
      client.enable = true;
      gpu = "amd";
      server.enable = true;
      serverAddr = "https://nasnix:6443";
      tokenFile = ./../secrets/k3s-token.yaml;
    };

    media = {
      enable = false;
      server.enable = false;
    };

    network.enable = true;
    nix = {
      atticPush.enable = true;
      enable = true;
    };
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
    zen-browser.enable = true;
    zsh.enable = true;
  };

  nixos.enable = true;

  pubkey = "";

  modules.homeManager =
    { pkgs, ... }:
    {
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

  modules.nixos =
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
            addresses = "192.168.0.25/24";
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
