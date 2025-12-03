{ ... }: {
  flake.types.generic.feature-options.sddm = { inputs, lib }:
    let inherit (inputs.self.types.generic) simpleFeature;
    in simpleFeature { inherit inputs lib; } "sddm feature";

  flake.modules.nixos.sddm-feature = { config, lib, ... }: {
    config = lib.mkIf config.host.features.sddm.enable {
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
