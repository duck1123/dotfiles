{ ... }:
{
  flake.nixidyApps.tailscale =
    {
      config,
      lib,
      pkgs,
      self,
      ...
    }:
    with lib;
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
        name = "tailscale";

        sopsSecrets = cfg: {
          tailscale-auth = {
            TS_AUTHKEY = cfg.oauth.authKey;
          };
          operator-oauth = with cfg.oauth; {
            client_id = clientId;
            client_secret = clientSecret;
          };
        };

        # https://tailscale.com/kb/1236/kubernetes-operator
        chart = helm.downloadHelmChart {
          repo = "https://pkgs.tailscale.com/helmcharts";
          chart = "tailscale-operator";
          version = "1.102.3";
          chartHash = "sha256-yPwHKq8qXGdZlbNi9Y7fFd4PKRt0tHUTuVuh+HsyBiY=";
        };

        extraOptions = {
          oauth = {
            # https://tailscale.com/kb/1185/kubernetes
            authKey = mkOption {
              description = mdDoc "The Tailscale auth key";
              type = types.str;
              default = "";
            };

            clientId = mkOption {
              description = mdDoc "The client id";
              type = types.str;
              default = "";
            };

            clientSecret = mkOption {
              description = mdDoc "The client secret";
              type = types.str;
              default = "";
            };
          };

          subnetRoutes = mkOption {
            description = mdDoc ''
              CIDR ranges the Tailscale Connector will advertise as subnet routes into the Tailnet.
              When non-empty, a `Connector` CRD is created that advertises these subnets so
              Tailscale clients can reach LAN IPs (e.g. MetalLB VIPs) without being on the local network.
              Approve the routes in the Tailscale admin console after first deploy.
            '';
            type = types.listOf types.str;
            default = [ ];
          };

          connectorHostname = mkOption {
            description = mdDoc "Tailscale machine name for the subnet-router Connector node.";
            type = types.str;
            default = "k3s-subnet-router";
          };

          connectorTags = mkOption {
            description = mdDoc ''
              ACL tags applied to the Connector node. Must match the tags permitted by your
              OAuth client in the Tailscale admin console (e.g. `tag:k8s-connector`).
            '';
            type = types.listOf types.str;
            default = [ "tag:k8s" ];
          };
        };

        extraAppConfig =
          cfg:
          lib.mkIf (cfg.subnetRoutes != [ ]) {
            yamls = [
              # ProxyClass puts the Connector pod into the host network namespace so it can
              # reach MetalLB L2 VIPs (192.168.0.240-250), which are ARP-based and unreachable
              # from inside the pod overlay network.
              ''
                apiVersion: tailscale.com/v1alpha1
                kind: ProxyClass
                metadata:
                  name: host-network
                  namespace: tailscale
                spec:
                  statefulSet:
                    pod:
                      hostNetwork: true
              ''
              ''
                apiVersion: tailscale.com/v1alpha1
                kind: Connector
                metadata:
                  name: ${cfg.connectorHostname}
                spec:
                  hostname: ${cfg.connectorHostname}
                  proxyClass: host-network
                  subnetRouter:
                    advertiseRoutes:
                ${lib.concatMapStringsSep "\n" (r: "                  - \"${r}\"") cfg.subnetRoutes}
                  tags:
                ${lib.concatMapStringsSep "\n" (t: "                  - ${t}") cfg.connectorTags}
              ''
            ];
          };

      };
}
