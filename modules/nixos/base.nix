{ ... }: {
  flake.modules.nixos.base = { inputs, ... }: {
    imports = [
      inputs.home-manager.nixosModules.home-manager
      inputs.sddm-sugar-candy-nix.nixosModules.default
      inputs.sops-nix.nixosModules.sops
      inputs.stylix.nixosModules.stylix
      ../../nixosModules
    ];
  };
}
