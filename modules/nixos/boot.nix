{ ... }:
{
  flake.modules.nixos.boot =
    {
      config,
      lib,
      pkgs,
      ...
    }:
    {
      options.boot.efiBoot = lib.mkOption {
        type = lib.types.bool;
        default = false;
        description = "Enable EFI boot configuration";
      };

      config = {
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
          plymouth.enable = true;
        }
        // lib.mkIf config.boot.efiBoot {
          boot.loader = {
            efi = {
              canTouchEfiVariables = true;
              efiSysMountPoint = "/boot";
            };
            grub = {
              enable = true;
              efiSupport = true;
              device = "nodev";
              useOSProber = lib.mkDefault false;
            };
          };
        };
      };
    };
}
