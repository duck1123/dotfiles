{ config, ... }:
let system = "x86_64-linux";
in {
  inspernix = import ./inspernix { inherit config system; };
  nasnix = import ./nasnix { inherit config system; };
  pixel8 = import ./pixel8 { inherit config system; };
  powerspecnix = import ./powerspecnix { inherit config system; };
  steamdeck = import ./steamdeck { inherit config system; };
  vavirl-pw0bwnq8 = import ./vavirl-pw0bwnq8 { inherit config system; };
}
