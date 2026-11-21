{
  homeManager =
    { config, lib, ... }:
    let
      # Same tailnet as clusters/dev/options.nix's tail-domain.
      tailnet-domain = "bearded-snake.ts.net";

      tailscale-hosts = lib.filterAttrs (
        _: host:
        host.features.tailscale.enable && host.features.ssh.enable && host.hostname != config.host.hostname
      ) config.hosts;
    in
    {
      programs.ssh = {
        enable = true;
        enableDefaultConfig = false;

        # Hosts with a static `lanIp` are reached by it directly; the rest go
        # via Tailscale MagicDNS instead of LAN mDNS/avahi, which is unreliable
        # across WiFi APs/VLANs and doesn't work off the LAN. `<host>-ts`
        # always goes via Tailscale, for reaching a lanIp host off the LAN.
        settings =
          lib.mapAttrs (_: host: {
            HostName = if host.lanIp != null then host.lanIp else "${host.hostname}.${tailnet-domain}";
            User = host.identity.username;
          }) tailscale-hosts
          // lib.mapAttrs' (
            name: host:
            lib.nameValuePair "${name}-ts" {
              HostName = "${host.hostname}.${tailnet-domain}";
              User = host.identity.username;
            }
          ) (lib.filterAttrs (_: host: host.lanIp != null) tailscale-hosts)
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

  nixos =
    { config, lib, ... }:
    let
      hostPubkeys = lib.filter (k: k != "") (lib.mapAttrsToList (_: h: h.pubkey) config.hosts);
    in
    {
      services.openssh = {
        enable = true;

        settings = {
          KbdInteractiveAuthentication = false;
          PasswordAuthentication = false;
        };
      };

      users.users.${config.host.identity.username}.openssh.authorizedKeys.keys = hostPubkeys;

      # So zsh on the remote end knows clients' $TERM (e.g. xterm-kitty).
      environment.enableAllTerminfo = true;
    };
}
