{ self, ... }:
{
  flake.nixidyApps.pihole =
    {
      config,
      lib,
      pkgs,
      self,
      ...
    }:
    with lib;
    let
      password-secret = "admin-password";
    in
    self.lib.mkArgoApp
      {
        inherit
          config
          lib
          self
          pkgs
          ;
      }
      {
        name = "pihole";

        sopsSecrets = cfg: { ${password-secret} = { inherit (cfg.auth) password; }; };

        # `pvcName = "pihole"` (not the usual "${name}-${name}-<key>" convention)
        # to match the chart's `existingClaim` value below, which is a literal
        # string the chart's own template plumbing already expects. Shape only
        # -- no volumeHandle here, that's environment-specific (see
        # env/dev/pihole.nix and docs/pinned-volumes.md).
        volumes = cfg: {
          data = {
            pvcName = "pihole";
            size = cfg.pvcSize;
          };
        };

        # https://artifacthub.io/packages/helm/mojo2600/pihole
        chart = lib.helm.downloadHelmChart {
          repo = "https://mojo2600.github.io/pihole-kubernetes/";
          chart = "pihole";
          version = "2.38.0";
          chartHash = "sha256-IxNqJfgw0lH9c9h5VcdvTCoIDHoWGdDOMYwEgTEHLc8=";
        };

        uses-ingress = true;

        extraOptions = {
          auth = {
            email = mkOption {
              description = mdDoc "Admin email";
              type = types.str;
              default = "admin@example.com";
            };

            password = mkOption {
              description = mdDoc "Web admin password";
              type = types.str;
              default = "CHANGEME";
            };
          };

          storageClass = mkOption {
            description = mdDoc "Storage class for the Pi-hole data PVC";
            type = types.str;
            default = "longhorn";
          };

          pvcSize = mkOption {
            description = mdDoc "Persistent volume size for Pi-hole configuration and data";
            type = types.str;
            default = "2Gi";
          };

          timezone = mkOption {
            description = mdDoc "Container TZ (Pi-hole / FTL)";
            type = types.str;
            default = "America/Detroit";
          };

          dns1 = mkOption {
            description = mdDoc "Upstream DNS server 1 (Pi-hole `DNS1` value)";
            type = types.str;
            default = "1.1.1.1";
          };

          dns2 = mkOption {
            description = mdDoc "Upstream DNS server 2 (Pi-hole `DNS2` value)";
            type = types.str;
            default = "1.0.0.1";
          };

          serviceDnsType = mkOption {
            description = mdDoc "`serviceDns.type` — use LoadBalancer with MetalLB so LAN clients can use a stable VIP for DNS";
            type = types.str;
            default = "LoadBalancer";
          };

          serviceDnsMixedService = mkOption {
            description = mdDoc "Single Service with TCP+UDP port 53 (recommended for LoadBalancer / MetalLB)";
            type = types.bool;
            default = true;
          };

          serviceDnsLoadBalancerIP = mkOption {
            description = mdDoc "Optional fixed MetalLB IP for the DNS service (null = pool assigns)";
            type = types.nullOr types.str;
            default = null;
          };

          customDnsEntries = mkOption {
            description = mdDoc ''
              Extra dnsmasq entries injected into Pi-hole. Use `address=/.domain/ip` for wildcard resolution.
              Example: `[ "address=/.local/192.168.0.241" ]` resolves all *.local to Traefik's MetalLB IP.
            '';
            type = types.listOf types.str;
            default = [ ];
          };

          serviceDhcpEnabled = mkOption {
            description = mdDoc "Expose DHCP `Service` (usually unnecessary in Kubernetes; disable unless you use Pi-hole DHCP through the cluster)";
            type = types.bool;
            default = false;
          };

          podDnsConfigEnabled = mkOption {
            description = mdDoc "Keep chart default pod DNS so the pod can resolve during bootstrap (uses 127.0.0.1 + fallback)";
            type = types.bool;
            default = true;
          };

          extraLinuxCapabilities = mkOption {
            description = mdDoc "Linux capabilities added to the Pi-hole container (`capabilities.add` in the Helm chart)";
            type = types.listOf types.str;
            default = [
              "SYS_TIME"
              "SYS_NICE"
            ];
          };
        };

        defaultValues =
          cfg:
          {
            admin = {
              enabled = true;
              existingSecret = password-secret;
              passwordKey = "password";
            };

            ingress = with cfg.ingress; {
              inherit ingressClassName;

              enabled = true;
              hosts = [ domain ];

              annotations = optionalAttrs (clusterIssuer != "") {
                "cert-manager.io/cluster-issuer" = clusterIssuer;
              };

              tls = [
                {
                  secretName = tls.secretName;
                  hosts = [ domain ];
                }
              ];
            };

            virtualHost = cfg.ingress.domain;

            persistentVolumeClaim = {
              accessModes = [ "ReadWriteOnce" ];
              enabled = true;
              size = cfg.pvcSize;
              storageClass = cfg.storageClassName;
              # Use our own statically-pinned PVC (see `pinnedVolumes` above)
              # instead of letting the chart generate a dynamically-provisioned
              # one -- this makes disabling/re-enabling pihole rebind to the
              # same existing data rather than getting a fresh empty volume.
              existingClaim = "pihole";
            };

            extraEnvVars = {
              TZ = cfg.timezone;
              FTLCONF_dns_listeningMode = "all";
            };

            DNS1 = cfg.dns1;
            DNS2 = cfg.dns2;

            dnsmasq.customDnsEntries = cfg.customDnsEntries;

            serviceDns = {
              type = cfg.serviceDnsType;
              mixedService = cfg.serviceDnsMixedService;
            }
            // optionalAttrs (cfg.serviceDnsLoadBalancerIP != null) {
              loadBalancerIP = cfg.serviceDnsLoadBalancerIP;
            };

            serviceDhcp = {
              enabled = cfg.serviceDhcpEnabled;
            };

            podDnsConfig = {
              enabled = cfg.podDnsConfigEnabled;
            };

            capabilities = optionalAttrs (cfg.extraLinuxCapabilities != [ ]) {
              add = cfg.extraLinuxCapabilities;
            };

            # Chart default probes call `/api/info/login` + `jq` (v5 API). Pi-hole v6 image often has no `jq` and a different API — probes never succeed.
            # Use built-in httpGet on `/admin` (chart template); HTTP 2xx/3xx counts as healthy.
            probes = {
              liveness = {
                type = "httpGet";
                port = "http";
                scheme = "HTTP";
                enabled = true;
                initialDelaySeconds = 90;
                failureThreshold = 5;
                timeoutSeconds = 5;
              };
              readiness = {
                type = "httpGet";
                port = "http";
                scheme = "HTTP";
                enabled = true;
                initialDelaySeconds = 30;
                failureThreshold = 6;
                timeoutSeconds = 5;
              };
            };

            resources = {
              requests = {
                cpu = "100m";
                memory = "256Mi";
              };
              limits = {
                cpu = "500m";
                memory = "512Mi";
              };
            };
          }
          // optionalAttrs (cfg.hostAffinity != null) {
            nodeSelector."kubernetes.io/hostname" = cfg.hostAffinity;
          };

        # The PVC/PV are now generated automatically from `pinnedVolumes` above.
      };
}
