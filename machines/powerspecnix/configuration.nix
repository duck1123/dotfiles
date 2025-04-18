{ config, inputs, pkgs, ... }:
let
  base = import ../../modules/base { inherit config inputs pkgs; };
  boot = import ../../modules/boot { inherit config inputs pkgs; };
  budgie = import ../../modules/budgie { inherit config inputs pkgs; };
  gnome = import ../../modules/gnome { inherit config inputs pkgs; };
  hyprland = import ../../modules/hyprland { inherit config inputs pkgs; };
  i18n = import ../../modules/i18n { inherit config inputs pkgs; };
  i3 = import ../../modules/i3 { inherit config inputs pkgs; };
  kubernetes = import ../../modules/kubernetes { inherit config inputs pkgs; };
  plasma6 = import ../../modules/plasma6 { inherit config inputs pkgs; };
  stylix = import ../../modules/stylix { inherit config inputs pkgs; };
  syncthing = import ../../modules/syncthing { inherit config inputs pkgs; };
  users = import ../../modules/users { inherit config inputs pkgs; };
  virtualization = import ../../modules/virtualization { inherit config inputs pkgs; };
  core = [
    ./hardware-configuration.nix
    base
    boot
    i18n
    kubernetes
    stylix
    syncthing
    users
    # virtualization
    inputs.home-manager.nixosModules.home-manager
    inputs.stylix.nixosModules.stylix
  ];
in {
  imports = core ++ [ hyprland ];

  # specialisation = {
  #   budgie = {
  #     inheritParentConfig = false;
  #     configuration.imports = core ++ [ budgie ];
  #   };
  #   gnome = {
  #     inheritParentConfig = false;
  #     configuration.imports = core ++ [ gnome ];
  #   };
  #   i3 = {
  #     inheritParentConfig = false;
  #     configuration.imports = core ++ [ i3 ];
  #   };
  #   hyprland = {
  #     inheritParentConfig = false;
  #     configuration.imports = core ++ [ hyprland ];
  #   };
  #   plasma6 = {
  #     inheritParentConfig = false;
  #     configuration.imports = core ++ [ plasma6 ];
  #   };
  # };
}
