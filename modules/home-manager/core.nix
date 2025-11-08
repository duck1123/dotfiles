{ ... }: {
  flake.modules.home-manager.core = { inputs, ... }:
    let hosts = import ../../hosts/default.nix { };
    in {
      imports = [
        inputs.self.modules.options.default
        inputs.stylix.homeModules.stylix
        inputs.zen-browser.homeModules.beta
        { inherit hosts; }
      ];
    };
}
