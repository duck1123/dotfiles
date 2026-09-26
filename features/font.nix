{
  nixos =
    { lib, pkgs, ... }:
    {
      # Configure console font
      console = {
        font = "ter-v32n";
        packages = with pkgs; [ terminus_font ];
        earlySetup = true;
      };

      # e.g. nerd-fonts.adwaita-mono, nerd-fonts.atkynson-mono, nerd-fonts.caskaydia-mono, nerd-fonts.fira-code, nerd-fonts.inconsolata
      fonts.packages = builtins.filter lib.attrsets.isDerivation (builtins.attrValues pkgs.nerd-fonts);
    };
}
