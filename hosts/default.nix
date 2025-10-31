{ identities, system, ... }: {
  inspernix = import ./inspernix { inherit system identities; };
  nasnix = import ./nasnix { inherit system identities; };
  pixel8 = import ./pixel8 { inherit system identities; };
  powerspecnix = import ./powerspecnix { inherit system identities; };
  steamdeck = import ./steamdeck { inherit system identities; };
  vallenpc = import ./vavirl-pw0bwnq8 { inherit system identities; };
}
