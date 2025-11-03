{ ... }: {
  flake.modules.home-manager.core = { inputs, ... }:
    let hosts = import ../../hosts/default.nix { };
    in {
      imports = [
        inputs.stylix.homeModules.stylix
        inputs.zen-browser.homeModules.beta
        { inherit hosts; }
        ../../nixosModules/flakeModules
      ];
    };
}
