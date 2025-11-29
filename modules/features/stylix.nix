{ ... }:
let image = ../../resources/wallpaper/nix-wallpaper-mosaic-blue.png;
in {
  flake.types.generic.feature-options.stylix = { inputs, lib }:
    with lib;
    let
      inherit (inputs.self.types) generic;
      simpleFeature = generic.simpleFeature { inherit inputs lib; };
    in simpleFeature "stylix feature";

  flake.modules.nixos.stylix-feature = { config, lib, pkgs, ... }: {
    config = lib.mkIf config.host.features.stylix.enable {
      stylix = {
        inherit image;
        enable = true;
        autoEnable = true;
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
    };
  };

  flake.modules.homeManager.stylix = { config, lib, pkgs, ... }: {
    config = lib.mkIf config.host.features.stylix.enable {
      stylix = {
        inherit image;
        enable = true;
        autoEnable = true;
        imageScalingMode = "fit";
        polarity = "dark";
        base16Scheme = "${pkgs.base16-schemes}/share/themes/3024.yaml";

        # cursor = {
        #   name = "Bibata-Modern-Ice";
        #   package = pkgs.bibata-cursors;
        # };

        targets.emacs.enable = false;
        targets.firefox.profileNames = [ "default" ];
        targets.vscode.profileNames = [ "default" ];
        targets.zen-browser.profileNames = [ "default" ];

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
  };
}
