{ config, inputs, pkgs, ... }:
let
  core = [
    ./hardware-configuration.nix
    ../../modules/base
    ../../modules/bitcoin
    ../../modules/boot
    ../../modules/i18n
    ../../modules/kubernetes
    ../../modules/nfs
    ../../modules/sddm
    ../../modules/stylix
    ../../modules/syncthing
    ../../modules/users
    # ../../modules/virtualization
    inputs.home-manager.nixosModules.home-manager
    inputs.stylix.nixosModules.stylix
  ];
in {
  imports = core ++ [ ../../modules/hyprland ];

  specialisation = {
    budgie = {
      inheritParentConfig = false;
      configuration.imports = core ++ [ ../../modules/budgie ];
    };
    gnome = {
      inheritParentConfig = false;
      configuration.imports = core ++ [ ../../modules/gnome ];
    };
    i3 = {
      inheritParentConfig = false;
      configuration.imports = core ++ [ ../../modules/i3 ];
    };
    hyprland = {
      inheritParentConfig = false;
      configuration.imports = core ++ [ ../../modules/hyprland ];
    };
    plasma6 = {
      inheritParentConfig = false;
      configuration.imports = core ++ [ ../../modules/plasma6 ];
    };
  };
}
