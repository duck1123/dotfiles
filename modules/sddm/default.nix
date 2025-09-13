{ host, lib, pkgs, ... }: {
  config = lib.mkIf host.features.sddm.enable {
    services.displayManager.sddm = {
      enable = true;
      wayland.enable = true;
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
}
