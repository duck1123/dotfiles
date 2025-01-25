# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ pkgs, stylix, ... }@inputs:
let
  config = {
    fullName = "Duck Nebuchadnezzar";
    hostname = "powerspecnix";
    username = "duck";
  };
  base = import ../../modules/base { inherit config inputs; };
  boot = import ../../modules/boot { inherit config inputs; };
  i18n = import ../../modules/i18n { inherit config inputs; };
  stylix = import ../../modules/stylix { inherit config inputs; };
  syncthing = import ../../modules/syncthing { inherit config inputs; };
  users = import ../../modules/users { inherit config inputs; };
in {
  imports = [
    ./hardware-configuration.nix
    base
    boot
    i18n
    stylix
    syncthing
    users
  ];

  specialisation = {
    # budgie = {
    #   inheritParentConfig = true;
    #   configuration.imports = [ ../../modules/budgie ];
    # };
    gnome = {
      inheritParentConfig = true;
      configuration.imports = [ ../../modules/gnome ];
    };
    # i3 = {
    #   inheritParentConfig = true;
    #   configuration.imports = [ ../../modules/i3 ];
    # };
    plasma6 = {
      inheritParentConfig = true;
      configuration.imports = [ ../../modules/plasma6 ];
    };
  };

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "23.11"; # Did you read the comment?
}
