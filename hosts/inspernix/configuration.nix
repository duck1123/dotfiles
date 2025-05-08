{ config, inputs, pkgs, ... }:
let
  inspernix-base =
    import ../../modules/inspernix-base { inherit config inputs pkgs; };
  boot = import ../../modules/boot { inherit config inputs pkgs; };
  gnome = import ../../modules/gnome { inherit config inputs pkgs; };
  hyprland = import ../../modules/hyprland { inherit config inputs pkgs; };
  i18n = import ../../modules/i18n { inherit config inputs pkgs; };
  stylix = import ../../modules/stylix { inherit config inputs pkgs; };
  syncthing = import ../../modules/syncthing { inherit config inputs pkgs; };
  core = [
    ./hardware-configuration.nix
    inspernix-base
    boot
    i18n
    stylix
    # syncthing
    inputs.home-manager.nixosModules.home-manager
    inputs.stylix.nixosModules.stylix
  ];
in {
  imports = core ++ [ hyprland ];
  specialisation.gnome = {
    inheritParentConfig = false;
    configuration.imports = core ++ [ gnome ];
  };
}
