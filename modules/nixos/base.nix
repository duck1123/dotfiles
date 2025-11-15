{ ... }: {
  flake.modules.nixos.base = { inputs, ... }:
    let features = with inputs.self.modules.nixos; [ battery-feature ];
    in {
      imports = features ++ [
        inputs.home-manager.nixosModules.home-manager
        inputs.sddm-sugar-candy-nix.nixosModules.default
        inputs.self.modules.generic.options
        inputs.sops-nix.nixosModules.sops
        inputs.stylix.nixosModules.stylix
        ../../nixosModules
      ];
    };
}
