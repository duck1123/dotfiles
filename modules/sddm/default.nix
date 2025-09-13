{ host, lib, pkgs, ... }: {
  config = lib.mkIf host.features.sddm.enable {
    environment.etc."sddm/themes/Breeze-Noir-Dark".source = pkgs.fetchFromGitHub {
      owner = "L4ki";
      repo = "Breeze-Noir-Dark";
      rev = "master";
      sha256 = "sha256-0GM3pmJ04hvoxuD7S0V7Wl/MP6kALiqTDWLdkLH6+/A=";
    };

    environment.etc."sddm/themes/mybreeze".source = pkgs.fetchFromGitHub {
      owner = "L4ki";
      repo = "MyBreeze-Plasma-Themes";
      rev = "main";
      sha256 = "sha256-0GM3pmJ04hvoxuD7S0V7Wl/MP6kALiqTDWLdkLH6+/A=";
    };

    services.displayManager.sddm = {
      enable = true;
      wayland.enable = true;
      theme = "Breeze-Noir-Dark";
      settings.Theme = {
        Font = "DejaVu Sans";
        FontSize = "14";
      };
    };
  };
}
