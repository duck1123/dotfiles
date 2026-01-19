{ ... }:
{
  flake.types.generic.feature-options.pictures =
    { inputs, lib }:
    let
      inherit (inputs.self.types.generic) simpleFeature;
    in
    simpleFeature { inherit inputs lib; } "pictures feature";

  flake.modules.homeManager.pictures =
    {
      config,
      lib,
      pkgs,
      ...
    }:
    {
      config = lib.mkIf config.host.features.pictures.enable {
        home.packages = with pkgs; [
          digikam
          gimp
          viewnior
        ];
      };
    };
}
