{ inputs, ... }:
let
  inherit (inputs.self.lib.mk-os) linux;
in
{
  flake.nixosConfigurations = {
    edgenix = linux "edgenix";
    inspernix = linux "inspernix";
    nasnix = linux "nasnix";
    powerspecnix = linux "powerspecnix";
    vidcentre = linux "vidcentre";
  };
}
