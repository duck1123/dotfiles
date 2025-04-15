{ inputs, pkgs, ... }: {
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
      efi.canTouchEfiVariables = true;
      systemd-boot.enable = true;
    };

    plymouth = {
      enable = true;
    };
  };
}
