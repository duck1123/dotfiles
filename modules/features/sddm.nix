{ ... }: {
  flake.modules.nixos.sddm-feature = { config, lib, pkgs, ... }: {
    config = lib.mkIf config.host.features.sddm.enable {
      services.displayManager.sddm = {
        enable = true;
        wayland.enable = true;
        package = pkgs.kdePackages.sddm;
        extraPackages = with pkgs; [ qt6.qtdeclarative qt6.qtsvg qt6.qt5compat ];
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
      };
    };
  };
}
