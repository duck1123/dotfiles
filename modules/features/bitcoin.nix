{ ... }:
let
  feature-name = "bitcoin";
in
{
  flake.types.generic.feature-options.${feature-name} =
    { inputs, lib }:
    let
      inherit (inputs.self.types.generic) simpleFeature;
    in
    simpleFeature { inherit inputs lib; } "bitcoin feature";

  flake.modules.homeManager.${feature-name} =
    {
      config,
      lib,
      pkgs,
      ...
    }:
    {
      config = lib.mkIf config.host.features.${feature-name}.enable {
        home.packages = with pkgs; [ sparrow ];
      };
    };

  flake.modules.nixos.${feature-name} =
    { config, lib, ... }:
    {
      config = lib.mkIf config.host.features.${feature-name}.enable {
        services.bitcoind.main = {
          enable = true;
          # dataDir = "/mnt/data3/bitcoin/bitcoind";
        };
      };
    };
}
