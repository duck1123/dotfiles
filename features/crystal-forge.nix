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

      agent.enable = lib.mkOption {
        type = lib.types.bool;
        default = true;
        description = ''
          Run the Crystal Forge agent, which reports this host's state to the
          server. The server won't queue a build for any system whose config
          doesn't enable an agent (a global, non-configurable policy). On
          first start it generates /var/lib/crystal-forge-agent/agent.key;
          register agent.pub's contents for this system in the UI. The agent
          can deploy, but only when the server asks, and systems default to
          the `manual` deployment policy.
        '';
      };

      port = lib.mkOption {
        type = lib.types.port;
        default = 3000;
        description = "Port the server listens on.";
      };

      serverHost = lib.mkOption {
        type = lib.types.str;
        # By IP: not every host resolves `nixmini` (nasnix doesn't).
        default = "192.168.0.25";
        description = "Server address agents connect to.";
      };

      serverUrl = lib.mkOption {
        type = lib.types.str;
        # HTTPS through the cluster proxy: over plain http the server refuses
        # to give the builder cache push credentials.
        default = "https://crystal-forge.home.kronkltd.net";
        description = "Server URL a builder connects to.";
      };
    };

  nixos =
    {
      config,
      inputs,
      lib,
      pkgs,
      ...
    }:
    let
      cfg = config.host.features.crystal-forge;
      cacheKeyFile = "/var/lib/crystal-forge/cache-encryption.key";
      # Units whose start scripts read cacheKeyFile (and exit without it).
      cacheKeyUsers = [
        "crystal-forge-server.service"
        "crystal-forge-hardening.service"
        "crystal-forge-config-inspector.service"
      ];
      agentKeyFile = "/var/lib/crystal-forge-agent/agent.key";
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

      # The server encrypts stored cache credentials (Attic tokens etc.) with
      # a key it hashes from this file; without it, saving a cache fails with
      # "missing cache encryption key". Generated on the host the first time,
      # like the builder's API key. Losing it only means re-entering cache
      # tokens in the UI.
      systemd.services.crystal-forge-cache-key = lib.mkIf cfg.server.enable {
        description = "Generate the Crystal Forge cache encryption key";
        before = cacheKeyUsers;
        requiredBy = cacheKeyUsers;
        serviceConfig = {
          Type = "oneshot";
          RemainAfterExit = true;
        };
        script = ''
          if [ ! -s ${cacheKeyFile} ]; then
            install -d -m 0750 -o crystal-forge -g crystal-forge /var/lib/crystal-forge
            umask 077
            ${lib.getExe' pkgs.openssl "openssl"} rand -base64 48 > ${cacheKeyFile}
            chown crystal-forge:crystal-forge ${cacheKeyFile}
            chmod 0400 ${cacheKeyFile}
          fi
        '';
      };

      # The agent's Ed25519 key, in the base64 format cf-keygen writes (as for
      # the builder's API key). cf-keygen puts the public half in agent.pub.
      systemd.services.crystal-forge-agent-key = lib.mkIf cfg.agent.enable {
        description = "Generate the Crystal Forge agent key";
        before = [ "crystal-forge-agent.service" ];
        requiredBy = [ "crystal-forge-agent.service" ];
        serviceConfig = {
          Type = "oneshot";
          RemainAfterExit = true;
        };
        script = ''
          if [ ! -s ${agentKeyFile} ]; then
            install -d -m 0750 /var/lib/crystal-forge-agent
            umask 077
            ${pkgs.crystal-forge.default.cf-keygen-drv}/bin/cf-keygen -y -f ${agentKeyFile}
            chmod 0644 /var/lib/crystal-forge-agent/agent.pub
          fi
        '';
      };

      # The module creates this user only with a server or builder, but its
      # tmpfiles rules (which apply whenever it's enabled) name it, so an
      # agent-only host needs it too.
      users.users.crystal-forge = lib.mkIf (!(cfg.server.enable || cfg.builder.enable)) {
        description = "Crystal Forge service user";
        isSystemUser = true;
        group = "crystal-forge";
        home = "/var/lib/crystal-forge";
      };

      services.crystal-forge = {
        enable = cfg.server.enable || cfg.builder.enable || cfg.agent.enable;

        client = lib.mkIf cfg.agent.enable {
          enable = true;
          server_host = cfg.serverHost;
          server_port = cfg.port;
          private_key = agentKeyFile;
        };
        local-database = cfg.server.enable;
        cache.encryption_key_file = lib.mkIf cfg.server.enable cacheKeyFile;

        server = {
          enable = cfg.server.enable;
          inherit (cfg) port;
          # The cache form's credential test refuses hosts that resolve to
          # private IPs ("Target blocked"), and our Attic
          # (attic.home.kronkltd.net) is on the LAN VIP.
          allow_private_cache_test_targets = true;

          # The server only hands cache push credentials to a builder over
          # verified HTTPS. Builders come in through Traefik (which sets
          # X-Forwarded-Proto itself) and the socat pod in
          # appTemplates/crystal-forge.nix, pinned to nixmini. The server sees
          # that pod's own IP, so trust nixmini's pod CIDR (`kubectl get node
          # nixmini -o jsonpath='{.spec.podCIDR}'`) rather than a /32 that
          # changes whenever the pod restarts.
          trust_forwarded_builder_https = true;
          trustedProxyCidrs = [ "10.42.3.0/24" ];
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
