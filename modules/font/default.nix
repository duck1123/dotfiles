{ host, lib, pkgs, ... }: {
  options = {
    features.font.enable = lib.mkOption {
      type = lib.types.bool;
      default = false;
      description = "Enable font";
    };
  };

  config = lib.mkIf host.features.font.enable {
    # Configure console font
    console = {
      font = "ter-v32n";
      packages = with pkgs; [ terminus_font ];
      earlySetup = true;
    };

    fonts.packages = builtins.filter lib.attrsets.isDerivation
      (builtins.attrValues pkgs.nerd-fonts);
  };
}
