{ pkgs, ... }: {
  environment.etc."sddm/themes/mybreeze".source = pkgs.fetchFromGitHub {
    owner = "L4ki";
    repo = "MyBreeze-Plasma-Themes";
    rev = "main";
    sha256 = "sha256-0GM3pmJ04hvoxuD7S0V7Wl/MP6kALiqTDWLdkLH6+/A=";
  };

  services.displayManager.sddm = {
    enable = true;
    wayland.enable = true;
    theme = "mybreeze";
    settings.Theme = {
      Font = "DejaVu Sans";
      FontSize = "14";
    };
  };
}
