{ ... }: {
  flake.types.generic.feature-options.nostr = { inputs, lib }:
    with lib;
    let
      inherit (inputs.self.types) generic;
      simpleFeature = generic.simpleFeature { inherit inputs lib; };
    in simpleFeature "nostr feature";

  flake.modules.homeManager.nostr = { config, lib, pkgs, ... }: {
    config = lib.mkIf config.host.features.nostr.enable {
      home.packages = with pkgs; [
        algia
        # gossip
        nak
        nostui
      ];
    };
  };
}

