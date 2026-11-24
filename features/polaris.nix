{
  description = "Polaris game streaming (a Sunshine fork) and the Moonlight client";

  extraOptions =
    { lib, ... }:
    {
      server.enable = lib.mkEnableOption ''
        the Polaris stream host. It runs as user services in the graphical
        session (a headless gamescope plus a private ScreenCast portal), so
        the host has to be logged in. Set up an account and pair clients in
        the web console at https://localhost:47990
      '';

      client.enable = lib.mkEnableOption ''
        Moonlight, to stream from a Polaris host. Pair it by entering the PIN
        it shows under Devices → Manual PIN in the host's web console
      '';
    };

  nixos =
    {
      config,
      inputs,
      lib,
      ...
    }:
    let
      cfg = config.host.features.polaris;
    in
    {
      # The upstream NixOS module (imported in modules/base.nix) handles
      # udev, uinput, the firewall, Avahi and the user's groups. The overlay
      # builds without CUDA unless nixpkgs.config.cudaSupport is set, which
      # is what an AMD host wants (VAAPI/Vulkan encoding).
      nixpkgs.overlays = lib.mkIf cfg.server.enable [ inputs.polaris.overlays.default ];

      services.polaris = lib.mkIf cfg.server.enable {
        enable = true;
        users = [ config.host.identity.username ];
      };

      # Moonlight finds hosts over mDNS; Avahi's openFirewall (on by default)
      # lets the replies in. A host can still be added by IP.
      services.avahi = lib.mkIf cfg.client.enable {
        enable = true;
        nssmdns4 = true;
      };
    };

  homeManager =
    {
      config,
      inputs,
      lib,
      pkgs,
      ...
    }:
    let
      cfg = config.host.features.polaris;
      # Standalone home-manager's pkgs has no overlays, so take the packages
      # from the same overlay the NixOS side uses (same derivations).
      polarisPkgs = pkgs.extend inputs.polaris.overlays.default;
    in
    {
      # The per-user units: polaris, its idle gamescope and the private
      # portal, all tied to graphical-session.target. Note it also puts a
      # plain pkgs.steam in the user profile, ahead of programs.steam's.
      services.polaris = lib.mkIf cfg.server.enable {
        enable = true;
        package = polarisPkgs.polaris-stream;
        packageGamescope = polarisPkgs.gamescope-polaris;
        packagePortal = polarisPkgs.xdg-desktop-portal-gamescope;
      };

      home.packages = lib.mkIf cfg.client.enable [ pkgs.moonlight-qt ];
    };
}
