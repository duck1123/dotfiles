{ config, ... }:
let system = "x86_64-linux";
in {
  inspernix = import ./inspernix.nix { inherit config system; };
  nasnix = import ./nasnix.nix { inherit config system; };
  pixel8 = import ./pixel8.nix { inherit config system; };
  powerspecnix = import ./powerspecnix.nix { inherit config system; };
  steamdeck = import ./steamdeck.nix { inherit config system; };
  vavirl-pw0bwnq8 = import ./vavirl-pw0bwnq8.nix { inherit config system; };
}
