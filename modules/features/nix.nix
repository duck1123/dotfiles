{ ... }:
{
  flake.types.generic.feature-options.nix =
    { inputs, lib }:
    let
      inherit (inputs.self.types.generic) simpleFeature;
    in
    simpleFeature { inherit inputs lib; } "nix feature";

  flake.modules.nixos.nix-feature =
    {
      config,
      inputs,
      lib,
      pkgs,
      ...
    }:
    {
      config = lib.mkIf config.host.features.nix.enable {
        environment.systemPackages = [
          inputs.attic.packages.${pkgs.stdenv.hostPlatform.system}.attic-client
        ];

        nix = {
          extraOptions = ''
            experimental-features = nix-command flakes
          '';

          gc = {
            automatic = true;
            options = "--delete-older-than 14d";
          };

          optimise.automatic = true;

          settings = {
            auto-optimise-store = true;
            experimental-features = [
              "nix-command"
              "flakes"
            ];
            substituters = [
              "https://duck1123.cachix.org"
              "https://hyprland.cachix.org"
              "https://nix-community.cachix.org"
              "https://attic.home.kronkltd.net/nixos"
            ];
            trusted-public-keys = [
              "duck1123.cachix.org-1:Cj3r3BH7Xuy0zFWy8V/VIB3F7+Gi1m9HB302E9UGV3E="
              "hyprland.cachix.org-1:a7pgxzMz7+chwVL3/pzj6jIBMioiJM7ypFP8PwtkuGc="
              "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
              "nixos:6s8iAyKEnH2z4spigUdDmt1VwiAwrvPA9vQNUd9if1k="
            ];
            trusted-users = [
              "root"
              "duck"
            ];
          };
        };

        nixpkgs.config.allowUnfree = true;
      };
    };
}
