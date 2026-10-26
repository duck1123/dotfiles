{
  extraOptions =
    { inputs, lib }:
    with lib;
    let
      inherit (inputs.self.types.generic) simpleFeature;
      # Defaults for the shared caches every host pulls from. A host can
      # override `substituters`/`trustedPublicKeys` in its own config to
      # diverge from this set.
      atticEndpoint = "https://attic.home.kronkltd.net";
      atticCache = "nixos";
    in
    {
      atticPush = simpleFeature {
        inherit inputs lib;
      } "automatic push-on-build to the Attic cache";

      # Off everywhere: Attic is the LAN cache, and pushing every build to the
      # public duck1123.cachix.org too was only a workaround for Attic being
      # unreliable. Push single paths by hand when needed
      # (`cachix push duck1123 <path>`).
      cachixPush = simpleFeature {
        inherit inputs lib;
      } "automatic push-on-build to duck1123.cachix.org (needs atticPush)";

      substituters = mkOption {
        type = types.listOf types.str;
        default = [
          "https://cache.nixos.org"
          "https://duck1123.cachix.org"
          "https://hyprland.cachix.org"
          "https://nix-community.cachix.org"
          "${atticEndpoint}/${atticCache}"
        ];
        description = "Nix binary cache substituter URLs for this host";
      };

      trustedPublicKeys = mkOption {
        type = types.listOf types.str;
        default = [
          "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
          "duck1123.cachix.org-1:Cj3r3BH7Xuy0zFWy8V/VIB3F7+Gi1m9HB302E9UGV3E="
          "hyprland.cachix.org-1:a7pgxzMz7+chwVL3/pzj6jIBMioiJM7ypFP8PwtkuGc="
          "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
          "nixos:/5T+7JIEApx8OL/j4HhK1koV6jMPu3rZV098GsuBAi4="
        ];
        description = "Trusted public keys matching `substituters`";
      };
    };

  homeManager =
    { config, pkgs, ... }:
    {
      # Used by scripts/nur.nu: nom for builds, statix for `nur lint`
      home.packages = [
        pkgs.nix-output-monitor
        pkgs.statix
      ];

      nix.package = pkgs.nix;

      nix.settings = {
        substituters = config.host.features.nix.substituters;
        trusted-public-keys = config.host.features.nix.trustedPublicKeys;
        fallback = true;
      };
    };

  nixos =
    {
      config,
      inputs,
      pkgs,
      ...
    }:
    let
      atticClient = inputs.attic.packages.${pkgs.stdenv.hostPlatform.system}.attic-client;
    in
    {
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
          substituters = config.host.features.nix.substituters;
          trusted-public-keys = config.host.features.nix.trustedPublicKeys;
          # Build locally when a substituter lists a path but can't deliver
          # it (e.g. Attic serving a narinfo, then failing the NAR download).
          fallback = true;
          trusted-users = [
            "root"
            config.host.identity.username
          ];
        };
      };

      nixpkgs.config.allowUnfree = true;
    };
}
