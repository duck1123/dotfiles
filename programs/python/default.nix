{ inputs, config, pkgs, ... }:
{
  home.packages = with pkgs; [
    # pip3
    python3
    python3Packages.pygame
  ];
}
