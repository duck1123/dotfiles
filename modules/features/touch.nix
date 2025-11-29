{ ... }: {
  flake.types.generic.feature-options.touch = { inputs, lib }:
    let inherit (inputs.self.types.generic) simpleFeature;
    in simpleFeature { inherit inputs lib; } "touch feature";

  flake.modules.nixos.touch-feature = { config, lib, ... }: {
    config = lib.mkIf config.host.features.touch.enable {
      services.libinput.enable = true;
    };
  };
}

