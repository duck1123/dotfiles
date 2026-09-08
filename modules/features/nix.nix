_: {
  flake.types.generic.feature-options.nix =
    { inputs, lib }:
    with lib;
    let
      inherit (inputs.self.types.generic) simpleFeature;
    in
    mkOption {
      type = types.submodule {
        options = {
          enable = mkOption {
            type = types.bool;
            default = false;
            description = "Enable nix feature";
          };
          atticPush = simpleFeature {
            inherit inputs lib;
          } "automatic push-on-build to the Attic cache via attic watch-store";
        };
      };
      default = { };
      description = "nix feature configuration";
    };

  flake.modules.nixos.nix-feature =
    {
      config,
      inputs,
      lib,
      pkgs,
      ...
    }:
    let
      atticClient = inputs.attic.packages.${pkgs.stdenv.hostPlatform.system}.attic-client;
      atticEndpoint = "https://attic.home.kronkltd.net";
      atticCache = "nixos";
    in
    {
      config = lib.mkMerge [
        (lib.mkIf config.host.features.nix.enable {
          environment.systemPackages = [ atticClient ];

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
              # `substituters`/`trusted-public-keys` here fully replace Nix's
              # built-in defaults rather than extending them, so cache.nixos.org
              # has to be listed explicitly -- otherwise, whenever the
              # self-hosted Attic cache is flaky, Nix has no other substitute
              # source and falls all the way back to compiling from source
              # (observed compiling gcc-4.6.4 bootstrap seeds from scratch).
              substituters = [
                "https://cache.nixos.org"
                "https://duck1123.cachix.org"
                "https://hyprland.cachix.org"
                "https://nix-community.cachix.org"
                "${atticEndpoint}/${atticCache}"
              ];
              trusted-public-keys = [
                "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
                "duck1123.cachix.org-1:Cj3r3BH7Xuy0zFWy8V/VIB3F7+Gi1m9HB302E9UGV3E="
                "hyprland.cachix.org-1:a7pgxzMz7+chwVL3/pzj6jIBMioiJM7ypFP8PwtkuGc="
                "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
                "nixos:6s8iAyKEnH2z4spigUdDmt1VwiAwrvPA9vQNUd9if1k="
              ];
              trusted-users = [
                "root"
                config.host.identity.username
              ];
            };
          };

          nixpkgs.config.allowUnfree = true;
        })

      ];
    };

  flake.modules.homeManager.nix-feature =
    {
      config,
      lib,
      pkgs,
      ...
    }:
    let
      atticEndpoint = "https://attic.home.kronkltd.net";
      atticCache = "nixos";
    in
    {
      config = lib.mkIf config.host.features.nix.enable {
        nix.package = pkgs.nix;

        nix.settings = {
          substituters = [
            "https://cache.nixos.org"
            "https://duck1123.cachix.org"
            "https://hyprland.cachix.org"
            "https://nix-community.cachix.org"
            "${atticEndpoint}/${atticCache}"
          ];
          trusted-public-keys = [
            "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
            "duck1123.cachix.org-1:Cj3r3BH7Xuy0zFWy8V/VIB3F7+Gi1m9HB302E9UGV3E="
            "hyprland.cachix.org-1:a7pgxzMz7+chwVL3/pzj6jIBMioiJM7ypFP8PwtkuGc="
            "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
            "nixos:6s8iAyKEnH2z4spigUdDmt1VwiAwrvPA9vQNUd9if1k="
          ];
        };
      };
    };
}
