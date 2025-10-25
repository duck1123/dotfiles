{ hosts, identities, inputs, system, ... }:
let inherit (inputs.nixpkgs.lib) nixosSystem;
in {
  inspernix = nixosSystem {
    inherit (hosts.inspernix) system;
    modules = [ ../hosts/inspernix/configuration.nix ];
    specialArgs = {
      inherit hosts identities inputs system;
      host = hosts.inspernix;
    };
  };

  nasnix = nixosSystem {
    inherit (hosts.inspernix) system;
    modules = [ ../hosts/nasnix/configuration.nix ];
    specialArgs = {
      inherit hosts identities inputs system;
      host = hosts.nasnix;
    };
  };

  powerspecnix = nixosSystem {
    inherit (hosts.powerspecnix) system;
    modules = [ ../hosts/powerspecnix/configuration.nix ];
    specialArgs = {
      inherit hosts identities inputs system;
      host = hosts.powerspecnix;
    };
  };
}
