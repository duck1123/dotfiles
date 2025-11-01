{ hosts, inputs, ... }:
let inherit (inputs.nixpkgs.lib) nixosSystem;
in {
  inspernix = nixosSystem {
    inherit (hosts.inspernix) system;
    modules = [
      {
        inherit hosts;
        host = hosts.inspernix;
      }
      ../hosts/inspernix/configuration.nix
    ];
    specialArgs = { inherit inputs; };
  };

  nasnix = nixosSystem {
    inherit (hosts.inspernix) system;
    modules = [
      {
        inherit hosts;
        host = hosts.nasnix;
      }
      ../hosts/nasnix/configuration.nix
    ];
    specialArgs = { inherit inputs; };
  };

  powerspecnix = nixosSystem {
    inherit (hosts.powerspecnix) system;
    modules = [
      {
        inherit hosts;
        host = hosts.powerspecnix;
      }
      ../hosts/powerspecnix/configuration.nix
    ];
    specialArgs = { inherit inputs; };
  };
}
