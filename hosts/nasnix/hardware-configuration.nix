{ lib, modulesPath, ... }: {
  imports = [ (modulesPath + "/profiles/qemu-guest.nix") ];

  boot.initrd.availableKernelModules =
    [ "ata_piix" "uhci_hcd" "virtio_pci" "sr_mod" "virtio_blk" ];
  boot.initrd.kernelModules = [ ];
  boot.kernelModules = [ "kvm-intel" ];
  boot.extraModulePackages = [ ];

  fileSystems."/" = {
    device = "/dev/disk/by-uuid/1f61c6bf-be89-48fe-8081-7a74ec707a38";
    fsType = "ext4";
  };

  # Add NFS mount for Videos share
  fileSystems."/mnt/videos" = {
    device = "192.168.0.124:/volume1/Videos";
    fsType = "nfs";
    options = [ "nfsvers=3" "ro" "hard" "timeo=600" "retrans=2" "_netdev" ];
  };

  swapDevices = [ ];
  networking.useDHCP = lib.mkDefault true;
  nixpkgs.hostPlatform = lib.mkDefault "x86_64-linux";
}
