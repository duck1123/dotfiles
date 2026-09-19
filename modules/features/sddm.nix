_: {
  features.sddm = {
    nixos = _: {
      services.displayManager.sddm = {
        enable = true;
        wayland.enable = true;
        sugarCandyNix = {
          enable = false;
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
