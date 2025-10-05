{ ... }: {
  flake.modules.nixos.sddm-feature = { config, lib, pkgs, ... }: {
    config = lib.mkIf config.host.features.sddm.enable {
      services.displayManager.sddm = {
        enable = true;
        wayland.enable = true;
        package = pkgs.libsForQt5.sddm;
        extraPackages = with pkgs; [ libsForQt5.qtgraphicaleffects ];
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
