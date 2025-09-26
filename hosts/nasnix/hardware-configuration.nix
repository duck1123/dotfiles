{ lib, modulesPath, ... }:
let nas-ip = "192.168.0.124";
in {
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
      options = [ "nfsvers=3" "ro" "hard" "timeo=600" "retrans=2" "_netdev" ];
    };

    "/mnt/downloads" = {
      device = "${nas-ip}:/volume1/Downloads";
      fsType = "nfs";
      options = [ "nfsvers=3" "ro" "hard" "timeo=600" "retrans=2" "_netdev" ];
    };

    "/mnt/music" = {
      device = "${nas-ip}:/volume1/Music";
      fsType = "nfs";
      options = [ "nfsvers=3" "ro" "hard" "timeo=600" "retrans=2" "_netdev" ];
    };

    "/mnt/videos" = {
      device = "${nas-ip}:/volume1/Videos";
      fsType = "nfs";
      options = [ "nfsvers=3" "ro" "hard" "timeo=600" "retrans=2" "_netdev" ];
    };
  };

  swapDevices = [ ];
  networking.useDHCP = lib.mkDefault true;
  nixpkgs.hostPlatform = lib.mkDefault "x86_64-linux";
}
