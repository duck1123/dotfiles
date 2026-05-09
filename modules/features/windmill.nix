{ ... }:
{
  flake.types.generic.feature-options.windmill =
    { inputs, lib }:
    let
      inherit (inputs.self.types.generic) simpleFeature;
    in
    simpleFeature { inherit inputs lib; } "windmill feature";

  flake.modules.homeManager.windmill =
    {
      config,
      inputs,
      lib,
      pkgs,
      ...
    }:
    {
      config = lib.mkIf config.host.features.windmill.enable {
        home.packages = [ inputs.self.packages.${pkgs.system}.windmill-cli ];

        xdg.configFile."fish/completions/wmill.fish".source = pkgs.runCommand "wmill-fish-completions" { } ''
          ${inputs.self.packages.${pkgs.system}.windmill-cli}/bin/wmill completions fish > $out
        '';
      };
    };
}
