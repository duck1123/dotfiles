{ ... }:
{
  flake.types.generic.feature-options.clojure =
    { inputs, lib }:
    let
      inherit (inputs.self.types.generic) simpleFeature;
    in
    simpleFeature { inherit inputs lib; } "clojure feature";

  flake.modules.homeManager.clojure =
    {
      config,
      lib,
      pkgs,
      ...
    }:
    {
      config = lib.mkIf config.host.features.clojure.enable {
        home.packages = with pkgs; [
          babashka
          bbin
          clojure
          clojure-lsp
          jet
        ];
      };
    };
}
