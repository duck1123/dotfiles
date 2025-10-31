{ host, lib, pkgs, ... }: {
  config = lib.mkIf host.features.stylix.enable {
    stylix = {
      enable = true;
      autoEnable = true;
      image = ./nix-wallpaper-mosaic-blue.png;
      imageScalingMode = "fit";
      polarity = "dark";
      # base16Scheme = "${pkgs.base16-schemes}/share/themes/3024.yaml";

      # cursor = {
      #   name = "Bibata-Modern-Ice";
      #   package = pkgs.bibata-cursors;
      # };

      targets.emacs.enable = true;
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
}
