{ inputs, ... }:
let inherit (inputs.self.lib.mk-os) linux;
in {
  flake.nixosConfigurations = {
    inspernix = linux "inspernix";
    nasnix = linux "nasnix";
    powerspecnix = linux "powerspecnix";
  };
}
