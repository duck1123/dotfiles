_: {
  flake.types.generic.feature-options.ssh =
    { inputs, lib }:
    let
      inherit (inputs.self.types.generic) simpleFeature;
    in
    simpleFeature { inherit inputs lib; } "ssh feature";

  flake.modules.nixos.ssh-feature =
    { config, lib, ... }:
    let
      hostPubkeys = lib.filter (k: k != "") (lib.mapAttrsToList (_: h: h.pubkey) config.hosts);
    in
    {
      config = lib.mkIf config.host.features.ssh.enable {
        services.openssh = {
          enable = true;

          settings = {
            KbdInteractiveAuthentication = false;
            PasswordAuthentication = false;
          };
        };

        users.users.${config.host.identity.username}.openssh.authorizedKeys.keys = hostPubkeys;
      };
    };

  flake.modules.homeManager.ssh-feature =
    { config, lib, ... }:
    let
      # Same tailnet as modules/kubernetes/_env/dev.nix's tail-domain.
      tailnet-domain = "bearded-snake.ts.net";

      tailscale-hosts = lib.filterAttrs (
        _: host:
        host.features.tailscale.enable && host.features.ssh.enable && host.hostname != config.host.hostname
      ) config.hosts;
    in
    {
      config = lib.mkIf config.host.features.ssh.enable {
        programs.ssh = {
          enable = true;
          enableDefaultConfig = false;

          # Aliases via Tailscale MagicDNS instead of LAN mDNS/avahi, which is
          # unreliable across WiFi APs/VLANs and doesn't work off the LAN.
          settings =
            lib.mapAttrs (_: host: {
              HostName = "${host.hostname}.${tailnet-domain}";
              User = host.identity.username;
            }) tailscale-hosts
            // {
              # Previous enableDefaultConfig defaults, kept explicitly since that
              # option is being deprecated upstream.
              "*" = {
                ForwardAgent = false;
                AddKeysToAgent = "no";
                Compression = false;
                ServerAliveInterval = 0;
                ServerAliveCountMax = 3;
                HashKnownHosts = false;
                UserKnownHostsFile = "~/.ssh/known_hosts";
                ControlMaster = "no";
                ControlPath = "~/.ssh/master-%r@%n:%p";
                ControlPersist = "no";
              };
            };
        };
      };
    };
}
