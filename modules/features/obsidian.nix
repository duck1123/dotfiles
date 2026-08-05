{ ... }:
{
  flake.types.generic.feature-options.obsidian =
    { inputs, lib }:
    let
      inherit (inputs.self.types.generic) simpleFeature;
    in
    simpleFeature { inherit inputs lib; } "obsidian feature";

  flake.modules.homeManager.obsidian =
    {
      config,
      lib,
      pkgs,
      ...
    }:
    {
      config = lib.mkIf config.host.features.obsidian.enable {
        home.packages = with pkgs; [ obsidian ];
      };
    };
}
