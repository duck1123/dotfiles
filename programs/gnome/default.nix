{ pkgs, ... }: {
  home.packages = with pkgs; [
    gnomeExtensions.appindicator
    gnomeExtensions.gsconnect
    # gnomeExtensions.topicons-plus
    # gnomeExtensions.tailscale-status
    guake
  ];
}

