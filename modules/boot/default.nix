{ pkgs, lib, config, ... }: {
  options.boot.efiBoot = lib.mkOption {
    type = lib.types.bool;
    default = false;
    description = "Enable EFI boot configuration";
  };

  config = {
    # Bootloader.
    boot = {
      binfmt.registrations.appimage = {
        interpreter = "${pkgs.appimage-run}/bin/appimage-run";
        magicOrExtension = "\\x7fELF....AI\\x02";
        mask = "\\xff\\xff\\xff\\xff\\x00\\x00\\x00\\x00\\xff\\xff\\xff";
        offset = 0;
        recognitionType = "magic";
        wrapInterpreterInShell = false;
      };

      kernelModules = [ "dm_crypt" ];

      loader = {
        efi.canTouchEfiVariables = false;

        grub = {
          device = "nodev";
          enable = true;
          useOSProber = lib.mkDefault false;
        };
      };

      plymouth.enable = true;
    };
  } // lib.mkIf config.boot.efiBoot {
    boot.loader = {
      efi = {
        canTouchEfiVariables = true;
        efiSysMountPoint = "/boot";
      };

    kernelModules = [ "dm_crypt" ];
    plymouth.enable = true;
  };
}
