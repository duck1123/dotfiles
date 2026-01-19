{ inputs, lib, ... }:
let
  mkNixos =
    system: cls: name:
    inputs.nixpkgs.lib.nixosSystem {
      inherit system;
      specialArgs = { inherit inputs; };
      modules = [
        {
          networking.hostName = lib.mkDefault name;
          nixpkgs.hostPlatform = lib.mkDefault system;
          system.stateVersion = "26.05";
        }
        inputs.self.modules.nixos.${cls}
        inputs.self.modules.nixos.${name}
      ];
    };

  mkDarwin =
    system: name:
    inputs.nix-darwin.lib.darwinSystem {
      inherit system;
      modules = [
        inputs.self.modules.darwin.darwin
        inputs.self.modules.darwin.${name}
        {
          networking.hostName = lib.mkDefault name;
          nixpkgs.hostPlatform = lib.mkDefault system;
          system.stateVersion = 6;
        }
      ];
    };
in
{
  flake.lib.mk-os = {
    inherit mkDarwin mkNixos;
    darwin = mkDarwin "aarch64-darwin";
    darwin-intel = mkDarwin "x86_64-darwin";
    linux = mkNixos "x86_64-linux" "nixos";
    linux-arm = mkNixos "aarch64-linux" "nixos";
    wsl = mkNixos "x86_64-linux" "wsl";
  };
}
