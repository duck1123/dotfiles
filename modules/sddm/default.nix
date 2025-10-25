{ host, lib, ... }: {
  options = {
    features.sddm.enable = lib.mkOption {
      type = lib.types.bool;
      default = false;
      description = "Enable sddm";
    };
  };

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
