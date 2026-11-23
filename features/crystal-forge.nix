{
  description = "Crystal Forge (NixOS fleet monitoring, compliance and build system)";

  extraOptions =
    { lib, ... }:
    {
      server.enable = lib.mkEnableOption ''
        the Crystal Forge server: API and web UI, flake watching, evaluation,
        and the hardening and config-inspector workers, with a local PostgreSQL
      '';

      builder.enable = lib.mkEnableOption ''
        a Crystal Forge builder in API mode. It needs no database; on first
        start it generates /var/lib/crystal-forge/builder-api.key and logs the
        public key, which has to be registered in the server's UI
      '';

      port = lib.mkOption {
        type = lib.types.port;
        default = 3000;
        description = "Port the server listens on.";
      };

      serverUrl = lib.mkOption {
        type = lib.types.str;
        default = "http://nixmini:3000";
        description = "Server URL a builder connects to.";
      };
    };

  nixos =
    {
      config,
      inputs,
      lib,
      ...
    }:
    let
      cfg = config.host.features.crystal-forge;
    in
    {
      # The upstream module expects its packages under pkgs.crystal-forge.
      # Take them from its own flake (its own nixpkgs pin) instead of
      # rebuilding them against ours through its overlay.
      nixpkgs.overlays = [
        (_: prev: {
          crystal-forge = {
            inherit (inputs.crystal-forge.packages.${prev.stdenv.hostPlatform.system})
              dashboards
              default
              run-postgres-jobs
              ;
          };
        })
      ];

      networking.firewall.allowedTCPPorts = lib.mkIf cfg.server.enable [ cfg.port ];

      services.crystal-forge = {
        enable = cfg.server.enable || cfg.builder.enable;
        local-database = cfg.server.enable;

        server = {
          enable = cfg.server.enable;
          inherit (cfg) port;
        };

        build = {
          enable = cfg.builder.enable;
          server_url = cfg.serverUrl;
        };

        flakes.watched = lib.mkIf cfg.server.enable [
          {
            name = "dotfiles";
            repo_url = "git+https://github.com/duck1123/dotfiles";
            branch = "master";
            auto_poll = true;
          }
        ];
      };
    };
}
