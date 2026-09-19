_: {
  # Options-only: declares `hosts.<host>.features.base`
  features.base = { };

  flake.modules = {
    homeManager.base =
      { inputs, ... }:
      let
        inherit (inputs.self.modules) generic homeManager;
      in
      {
        imports = [
          homeManager.state-version
          homeManager.environments-gnome
          generic.options
          inputs.stylix.homeModules.stylix
          inputs.zen-browser.homeModules.beta
        ]
        # Every feature registered via `features.<name>` (see modules/flake/features.nix)
        ++ builtins.attrValues homeManager.features;
      };

    nixos.base =
      { inputs, ... }:
      let
        inherit (inputs.self.modules) generic nixos;
      in
      {
        imports = [
          nixos.state-version
          nixos.boot
          nixos.i18n
          nixos.nix-attic
          nixos.sddm
          nixos.users
          generic.options
          inputs.home-manager.nixosModules.home-manager
          inputs.sddm-sugar-candy-nix.nixosModules.default
          inputs.sops-nix.nixosModules.sops
          inputs.stylix.nixosModules.stylix
        ]
        ++ builtins.attrValues nixos.features;
      };
  };
}
