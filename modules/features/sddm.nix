{ ... }: {
  flake.modules.nixos.sddm-feature = { config, lib, pkgs, ... }: {
    config = lib.mkIf config.host.features.sddm.enable {
      services.displayManager.sddm = {
        enable = true;
        package = pkgs.kdePackages.sddm;

        settings.Theme = {
          Current = "sddm-sugar-candy-nix";
          ThemeDir = "/run/current-system/sw/share/sddm/themes";
        };

        sugarCandyNix = {
          enable = true;
          settings = {
            Font = "DejaVu Sans";
            FontSize = "14";
            FormPosition = "center";
            MainColor = "blue";
            PartialBlur = true;
          };
        };

        theme = lib.mkForce "sddm-sugar-candy-nix";
        wayland.enable = true;
      };
    };
  };
}
