{ inputs, ... }:
let
  inherit (inputs.self.lib.mk-os) wsl linux;
in
{
  flake.nixosConfigurations = {
    edgenix = linux "edgenix";
    inspernix = linux "inspernix";
    nasnix = linux "nasnix";
    powerspecnix = linux "powerspecnix";
    vidcentre = linux "vidcentre";
    # vavirl-pw0bwnq8 = wsl "vavirl-pw0bwnq8";
  };
}
