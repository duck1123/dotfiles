{ ... }: {
  flake.types.generic.feature-options.nostr = { inputs, lib }:
    let inherit (inputs.self.types.generic) simpleFeature;
    in simpleFeature { inherit inputs lib; } "nostr feature";

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

