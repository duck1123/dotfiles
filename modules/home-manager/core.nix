{ ... }: {
  flake.modules.home-manager.core = { inputs, ... }:
    let hosts = import ../../hosts/default.nix { };
    in {
      inherit hosts;

      imports = [
        inputs.self.modules.options.default
        inputs.stylix.homeModules.stylix
        inputs.zen-browser.homeModules.beta
      ];
    };
}
