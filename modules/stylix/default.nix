{ inputs, pkgs, ... }:
let stylix = inputs.stylix;
in {
  stylix = {
    enable = true;
    autoEnable = true;
    # image = config.lib.stylix.pixel "base0A";
    image = ../../machines/powerspecnix/nix-wallpaper-mosaic-blue.png;
    imageScalingMode = "fit";
    polarity = "dark";
    base16Scheme = "${pkgs.base16-schemes}/share/themes/3024.yaml";
    # base16Scheme = "${pkgs.base16-schemes}/share/themes/catppuccin-frappe.yaml";
    # base16Scheme = "${pkgs.base16-schemes}/share/themes/catppuccin-latte.yaml";
    # base16Scheme = "${pkgs.base16-schemes}/share/themes/evenok-dark.yaml";
    # base16Scheme = "${pkgs.base16-schemes}/share/themes/still-alive.yaml";

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
}
