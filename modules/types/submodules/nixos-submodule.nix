{ ... }:
{
  flake.types.generic.nixos-submodule =
    { inputs, lib, ... }:
    let
      inherit (inputs.self.types) generic;
      simpleFeature = generic.simpleFeature { inherit inputs lib; };
    in
    with lib;
    types.submodule {
      options = {
        enable = mkOption {
          type = types.bool;
          default = false;
          description = "Enable nixos configuration";
        };

        budgie = simpleFeature "budgie environment";
        gnome = simpleFeature "gnome environment";
        hyprland = simpleFeature "hyprland environment";
        i3 = simpleFeature "i3 environment";
        plasma6 = simpleFeature "plasma6 environment";
      };
    };
}
