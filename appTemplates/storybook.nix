{ lib, self, ... }:
with lib;
let
  name = "storybook";
  labels = {
    "app.kubernetes.io/name" = name;
  };

  # ── Runtime ──────────────────────────────────────────────────────────────
  # Storybook for the shared dashboard UI components
  # (resources/apps/dashboard-ui/), built by the `storybook-runtime` flake
  # package (modules/kubernetes/pkgs/dashboard-ui.nix): python3 plus the
  # static site at share/storybook. Same pattern as appTemplates/duck1123.nix:
  # the store path is resolved at `nur switch k8s` time and mounted by
  # nixkube by store path, and k8s-switch-charts.sh pushes it to Attic. See
  # modules/kubernetes/docs/static-sites.md.
  runtime = self.packages.x86_64-linux.storybook-runtime;
in
{
  inherit name;
  uses-ingress = true;

  extraResources =
    cfg:
    let
      port = 8080;
    in
    {
      deployments.${name}.spec = {
        selector.matchLabels = labels;
        template = {
          metadata.labels = labels;
          spec = {
            containers = [
              {
                inherit name;
                image = "ghcr.io/lillecarl/nix-csi/scratch:1.0.1";
                command = [
                  "python3"
                  "-m"
                  "http.server"
                  (toString port)
                  "--directory"
                  "/nix/var/result/share/storybook"
                ];
                ports = [
                  {
                    containerPort = port;
                    name = "http";
                    protocol = "TCP";
                  }
                ];
                readinessProbe.httpGet = {
                  path = "/";
                  port = "http";
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
                  volumeAttributes."x86_64-linux" = "${runtime}";
                };
              }
            ];
          };
        };
      };

      ingresses.${name} = {
        metadata.annotations."cert-manager.io/cluster-issuer" = cfg.ingress.clusterIssuer;
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
              secretName = "${name}-tls";
            }
          ];
        };
      };

      services.${name}.spec = {
        selector = labels;
        ports = [
          {
            name = "http";
            port = 80;
            targetPort = port;
            protocol = "TCP";
          }
        ];
      };
    };
}
