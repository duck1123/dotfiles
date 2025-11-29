{ ... }: {
  flake.types.generic.feature-options.bitcoin = { inputs, lib }:
    with lib;
    let
      inherit (inputs.self.types) generic;
      simpleFeature = generic.simpleFeature { inherit inputs lib; };
    in simpleFeature "bitcoin feature";

  flake.modules.nixos.bitcoin-feature = { config, lib, ... }: {
    config = lib.mkIf config.host.features.bitcoin.enable {
      services.bitcoind.main = {
        enable = true;
        # dataDir = "/mnt/data3/bitcoin/bitcoind";
      };
    };
  };
}

