{ ... }:
let
  image = ../../resources/wallpaper/nix-wallpaper-mosaic-blue.png;

  # https://tinted-theming.github.io/tinted-gallery/
  theme = "3024";
  # theme = "catppuccin-frappe";
  # theme = "catppuccin-latte";
  # theme = "evenok-dark";
  # theme = "humanoid-dark";
  # theme = "oxocarbon-dark";
  # theme = "still-alive";
in
{
  flake.types.generic.feature-options.stylix =
    { inputs, lib }:
    let
      inherit (inputs.self.types.generic) simpleFeature;
    in
    simpleFeature { inherit inputs lib; } "stylix feature";

  flake.modules.nixos.stylix-feature =
    {
      config,
      lib,
      pkgs,
      ...
    }:
    {
      config = lib.mkIf config.host.features.stylix.enable {
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
    };

  flake.modules.homeManager.stylix =
    {
      config,
      lib,
      pkgs,
      ...
    }:
    {
      config = lib.mkIf config.host.features.stylix.enable {
        stylix = {
          inherit image;
          enable = true;
          autoEnable = true;
          imageScalingMode = "fit";
          polarity = "dark";
          base16Scheme = "${pkgs.base16-schemes}/share/themes/${theme}.yaml";

          # cursor = {
          #   name = "Bibata-Modern-Ice";
          #   package = pkgs.bibata-cursors;
          # };

          targets.emacs.enable = false;
          targets.firefox.profileNames = [ "default" ];
          targets.hyprland.enable = false;
          targets.vscode.profileNames = [ "default" ];
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
