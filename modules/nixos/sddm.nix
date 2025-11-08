{ ... }: {
  flake.modules.nixos.sddm = { inputs, ... }: {
    nixpkgs.overlays = [ inputs.sddm-sugar-candy-nix.overlays.default ];
  };
}
