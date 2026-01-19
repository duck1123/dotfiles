{ ... }:
{
  flake.types.generic.feature-options.font =
    { inputs, lib }:
    let
      inherit (inputs.self.types.generic) simpleFeature;
    in
    simpleFeature { inherit inputs lib; } "font feature";

  flake.modules.nixos.font-feature =
    {
      config,
      lib,
      pkgs,
      ...
    }:
    {
      config = lib.mkIf config.host.features.font.enable {
        # Configure console font
        console = {
          font = "ter-v32n";
          packages = with pkgs; [ terminus_font ];
          earlySetup = true;
        };

        fonts.packages =
          with pkgs;
          [
            # nerd-fonts.adwaita-mono
            # nerd-fonts.atkynson-mono
            # nerd-fonts.caskaydia-mono
            # nerd-fonts.fira-code
            # nerd-fonts.inconsolata
          ]
          ++ builtins.filter lib.attrsets.isDerivation (builtins.attrValues pkgs.nerd-fonts);
      };
    };
}
