{ inputs, ... }: {
  flake.modules.nixos.wsl =
    { inputs, ... }:
    {
      imports = [
        inputs.nixos-wsl.nixosModules.wsl
        inputs.self.modules.nixos.state-version
        inputs.self.modules.nixos.i18n
        inputs.self.modules.nixos.nix-feature
        inputs.self.modules.nixos.users
        inputs.self.modules.nixos.zsh-feature
        inputs.self.modules.generic.options
        inputs.sops-nix.nixosModules.sops
      ];

      wsl.enable = true;
    };
}
