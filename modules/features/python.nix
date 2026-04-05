{ ... }:
let
  name = "python";
in
{
  flake.types.generic.feature-options.${name} =
    { inputs, lib }:
    let
      inherit (inputs.self.types.generic) simpleFeature;
    in
      simpleFeature { inherit inputs lib; } "${name} feature";

  flake.modules.homeManager.${name} =
    {
      config,
      lib,
      pkgs,
      ...
    }:
    {
      config = lib.mkIf config.host.features.${name}.enable {
        home.packages = with pkgs; [
          python3
        ];
      };
    };
}
