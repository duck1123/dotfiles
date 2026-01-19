{ ... }:
{
  flake.types.generic.feature-options.chm =
    { inputs, lib }:
    let
      inherit (inputs.self.types.generic) simpleFeature;
    in
    simpleFeature { inherit inputs lib; } "chm feature";

  flake.modules.homeManager.chm =
    {
      config,
      lib,
      pkgs,
      ...
    }:
    {
      config = lib.mkIf config.host.features.chm.enable {
        home.packages = with pkgs; [ kchmviewer ];
      };
    };
}
