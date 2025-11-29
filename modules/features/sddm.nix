{ ... }: {
  flake.types.generic.feature-options.sddm = { inputs, lib }:
    with lib;
    let
      inherit (inputs.self.types) generic;
      simpleFeature = generic.simpleFeature { inherit inputs lib; };
    in simpleFeature "sddm feature";

  flake.modules.nixos.sddm-feature = { config, lib, pkgs, ... }: {
    config = lib.mkIf config.host.features.sddm.enable {
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
  };
}

