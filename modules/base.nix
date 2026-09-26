_: {
  flake.modules = {
    homeManager.base =
      {
        config,
        inputs,
        lib,
        ...
      }:
      let
        inherit (inputs.self.modules) generic homeManager;
      in
      {
        imports = [
          homeManager.state-version
          generic.options
          inputs.stylix.homeModules.stylix
          inputs.zen-browser.homeModules.beta
        ]
        # Every feature registered via `features.<name>` (see modules/flake/features.nix)
        # and environment via `environments.<name>` (see modules/flake/environments.nix)
        ++ builtins.attrValues homeManager.features
        ++ builtins.attrValues homeManager.environments;

        gtk.gtk4.theme = lib.mkDefault config.gtk.theme;
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
          nixos.specialisations
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
