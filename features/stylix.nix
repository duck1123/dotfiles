let
  # https://tinted-theming.github.io/tinted-gallery/
  # theme = "3024";
  # theme = "catppuccin-frappe";
  # theme = "catppuccin-latte";
  # theme = "evenok-dark";
  # theme = "humanoid-dark";
  theme = "oxocarbon-dark";
  # theme = "still-alive";
  image = ../resources/wallpaper/nix-wallpaper-mosaic-blue.png;
in
{
  nixos =
    { pkgs, ... }:
    {
      stylix = {
        inherit image;
        autoEnable = true;
        base16Scheme = "${pkgs.base16-schemes}/share/themes/${theme}.yaml";
        enable = true;
        imageScalingMode = "fit";
        polarity = "dark";

        # cursor = {
        #   name = "Bibata-Modern-Ice";
        #   package = pkgs.bibata-cursors;
        # };

        fonts = {
          # monospace = {
          #   package = pkgs.nerdfonts.override {fonts = ["JetBrainsMono"];};
          #   name = "JetBrainsMono Nerd Font Mono";
          # };
          sansSerif = {
            package = pkgs.dejavu_fonts;
            name = "DejaVu Sans";
          };
          serif = {
            package = pkgs.dejavu_fonts;
            name = "DejaVu Serif";
          };
        };
      };
    };

  homeManager =
    { pkgs, ... }:
    {
      stylix = {
        inherit image;
        autoEnable = true;
        base16Scheme = "${pkgs.base16-schemes}/share/themes/${theme}.yaml";

        # cursor = {
        #   name = "Bibata-Modern-Ice";
        #   package = pkgs.bibata-cursors;
        # };

        enable = true;

        fonts = {
          # monospace = {
          #   package = pkgs.nerdfonts.override {fonts = ["JetBrainsMono"];};
          #   name = "JetBrainsMono Nerd Font Mono";
          # };
          sansSerif = {
            package = pkgs.dejavu_fonts;
            name = "DejaVu Sans";
          };
          serif = {
            package = pkgs.dejavu_fonts;
            name = "DejaVu Serif";
          };
        };

        imageScalingMode = "fit";
        polarity = "dark";

        targets = {
          firefox.profileNames = [ "default" ];
          hyprland.enable = false;
          vscode.profileNames = [ "default" ];
        };
      };
    };
}
