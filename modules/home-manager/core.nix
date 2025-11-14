{ ... }: {
  flake.modules.homeManager.core = { inputs, ... }:
    let hosts = import ../../hosts/default.nix { };
    in {
      inherit hosts;

      imports = [
        inputs.self.modules.generic.options
        inputs.stylix.homeModules.stylix
        inputs.zen-browser.homeModules.beta
      ];
    };
}
