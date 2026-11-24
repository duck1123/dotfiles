{ lib, pkgs, ... }:
with lib;
let
  name = "crystal-forge";
  labels."app.kubernetes.io/name" = name;
  port = 3000;
in
{
  # Crystal Forge itself runs as a NixOS service on a host
  # (features/crystal-forge.nix), not in the cluster. This app only gives it
  # an HTTPS name under the cluster's Traefik: its session cookie is always
  # `Secure`, so browsers drop it over plain http://<host>:3000 and logins
  # never stick.
  #
  # A selectorless Service with hand-written Endpoints/EndpointSlice would be
  # the usual way to point at an outside host, but ArgoCD's default
  # resource.exclusions skip both kinds, so they'd never be applied. Instead
  # a socat pod forwards TCP to the host; Traefik terminates TLS in front of it.
  inherit name;
  uses-ingress = true;

  extraOptions.upstream = {
    host = mkOption {
      description = mdDoc "Address of the host running the Crystal Forge server.";
      type = types.str;
    };

    port = mkOption {
      description = mdDoc "Port the Crystal Forge server listens on.";
      type = types.port;
      default = 3000;
    };
  };

  extraResources = cfg: {
    deployments.${name}.spec = {
      selector.matchLabels = labels;
      template = {
        metadata.labels = labels;
        spec = {
          # Pinned (hostAffinity) to the host running the server, so the
          # server sees this pod's connections from that node's pod CIDR, which
          # it trusts for X-Forwarded-Proto (features/crystal-forge.nix,
          # trustedProxyCidrs). Builders need that to receive cache push
          # credentials.
          nodeSelector = optionalAttrs (cfg.hostAffinity != null) {
            "kubernetes.io/hostname" = cfg.hostAffinity;
          };
          containers = [
            {
              inherit name;
              image = "ghcr.io/lillecarl/nix-csi/scratch:1.0.1";
              command = [ "socat" ];
              args = [
                "TCP-LISTEN:${toString port},fork,reuseaddr"
                "TCP:${cfg.upstream.host}:${toString cfg.upstream.port}"
              ];
              ports = [
                {
                  containerPort = port;
                  name = "http";
                  protocol = "TCP";
                }
              ];
              resources = {
                requests = {
                  cpu = "10m";
                  memory = "16Mi";
                };
                limits.memory = "64Mi";
              };
              volumeMounts = [
                {
                  name = "nix";
                  mountPath = "/nix";
                  subPath = "nix";
                }
              ];
            }
          ];
          volumes = [
            {
              name = "nix";
              csi = {
                driver = "nixkube";
                volumeAttributes."x86_64-linux" = "${pkgs.socat}";
              };
            }
          ];
        };
      };
    };

    services.${name}.spec = {
      selector = labels;
      ports = [
        {
          name = "http";
          inherit port;
          targetPort = port;
          protocol = "TCP";
        }
      ];
    };

    ingresses.${name} = {
      metadata.annotations = optionalAttrs (cfg.ingress.clusterIssuer != "") {
        "cert-manager.io/cluster-issuer" = cfg.ingress.clusterIssuer;
      };

      spec = with cfg.ingress; {
        inherit ingressClassName;

        rules = [
          {
            host = domain;
            http.paths = [
              {
                backend.service = {
                  inherit name;
                  port.name = "http";
                };
                path = "/";
                pathType = "ImplementationSpecific";
              }
            ];
          }
        ];

        tls = [
          {
            hosts = [ domain ];
            secretName = "${domain}-tls";
          }
        ];
      };
    };
  };
}
