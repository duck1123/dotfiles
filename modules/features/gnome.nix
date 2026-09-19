_: {
  features.gnome = {
    homeManager =
      { pkgs, ... }:
      {
        home.packages = with pkgs; [
          gnomeExtensions.appindicator
          gnomeExtensions.gsconnect
          # gnomeExtensions.topicons-plus
          guake
        ];
      };
  };
}
