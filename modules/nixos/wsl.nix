{ inputs, ... }: {
  flake.modules.nixos.wsl =
    { inputs, ... }:
    {
      imports = [
        inputs.nixos-wsl.nixosModules.wsl
        inputs.self.modules.nixos.state-version
        inputs.self.modules.nixos.i18n
        inputs.self.modules.nixos.features.nix
        inputs.self.modules.nixos.users
        inputs.self.modules.nixos.features.zsh
        inputs.self.modules.generic.options
      ];

      wsl.enable = true;
    };
}
