{ ... }: {
  flake.types.generic.feature-options.xserver = { inputs, lib }:
    with lib;
    let
      inherit (inputs.self.types) generic;
      simpleFeature = generic.simpleFeature { inherit inputs lib; };
    in simpleFeature "xserver feature";

  flake.modules.nixos.xserver-feature = { config, lib, ... }: {
    config = lib.mkIf config.host.features.xserver.enable {
      services.xserver = {
        enable = true;

        xkb = {
          layout = "us";
          variant = "";
        };
      };

      xdg.portal = {
        enable = true;
        config.common.default = "*";
      };
    };
  };
}

