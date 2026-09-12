{ ... }:
{
  flake.nixidyApps.pinchflat =
    {
      config,
      lib,
      self,
      ...
    }:
    with lib;
    self.lib.mkArgoApp { inherit config lib self; } (
      let
        name = "pinchflat";
        labels."app.kubernetes.io/name" = name;
        secretKeyBaseSecret = "pinchflat-secret-key-base";
      in
      {
        inherit name;
        uses-ingress = true;
        uses-nfs = true;

        # Small precious volume (SQLite db + logs/metadata) -- see
        # docs/pinned-volumes.md if this ever needs pinning across an
        # enable=false -> true cycle.
        volumes = cfg: {
          config.size = "2Gi";
        };

        extraOptions = {
          image = mkOption {
            description = mdDoc "The pinchflat docker image";
            type = types.str;
            default = "ghcr.io/kieraneglin/pinchflat:latest";
          };

          service.port = mkOption {
            description = mdDoc "The service port";
            type = types.int;
            default = 8945;
          };

          secretKeyBase = mkOption {
            description = mdDoc ''
              SECRET_KEY_BASE for signing/encrypting cookies. Pinchflat falls
              back to a hardcoded default shared by every self-hosted install
              if this is empty -- set it (secrets.pinchflat.secretKeyBase) to
              avoid that.
            '';
            type = types.str;
            default = "";
          };
        };

        sopsSecrets =
          cfg:
          optionalAttrs (cfg.secretKeyBase != "") {
            ${secretKeyBaseSecret}.SECRET_KEY_BASE = cfg.secretKeyBase;
          };

        extraResources = cfg: {
          deployments.${name}.spec = {
            selector.matchLabels = labels;
            template = {
              metadata.labels = labels;
              spec = {
                containers = [
                  {
                    inherit name;
                    image = cfg.image;
                    imagePullPolicy = "IfNotPresent";
                    env = lib.optionals (cfg.secretKeyBase != "") [
                      {
                        name = "SECRET_KEY_BASE";
                        valueFrom.secretKeyRef = {
                          name = secretKeyBaseSecret;
                          key = "SECRET_KEY_BASE";
                        };
                      }
                    ];
                    ports = [
                      {
                        containerPort = cfg.service.port;
                        name = "http";
                        protocol = "TCP";
                      }
                    ];
                    readinessProbe = {
                      httpGet = {
                        path = "/healthcheck";
                        port = cfg.service.port;
                      };
                      initialDelaySeconds = 15;
                      periodSeconds = 10;
                      timeoutSeconds = 5;
                      failureThreshold = 6;
                    };
                    livenessProbe = {
                      httpGet = {
                        path = "/healthcheck";
                        port = cfg.service.port;
                      };
                      initialDelaySeconds = 30;
                      periodSeconds = 30;
                      timeoutSeconds = 5;
                      failureThreshold = 3;
                    };
                    resources = {
                      requests = {
                        memory = "256Mi";
                        cpu = "100m";
                      };
                      limits = {
                        memory = "1Gi";
                        cpu = "1000m";
                      };
                    };
                    volumeMounts = [
                      {
                        name = "config";
                        mountPath = "/config";
                      }
                      {
                        name = "downloads";
                        mountPath = "/downloads";
                      }
                    ];
                  }
                ];
                volumes = [
                  cfg.volumes.config.volume
                  {
                    name = "downloads";
                    persistentVolumeClaim.claimName = "${name}-${name}-downloads";
                  }
                ];
              };
            };
          };

          ingresses.${name} = with cfg.ingress; {
            metadata.annotations = optionalAttrs (clusterIssuer != "") {
              "cert-manager.io/cluster-issuer" = clusterIssuer;
            };

            spec = {
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

          services.${name}.spec = {
            selector = labels;
            ports = [
              {
                name = "http";
                port = cfg.service.port;
                targetPort = cfg.service.port;
                protocol = "TCP";
              }
            ];
          };

          persistentVolumeClaims."${name}-${name}-downloads".spec =
            if cfg.nfs.enable then
              {
                accessModes = [ "ReadWriteMany" ];
                resources.requests.storage = "1Gi";
                storageClassName = "";
                volumeName = "${name}-${name}-downloads-nfs";
              }
            else
              {
                inherit (cfg) storageClassName;
                accessModes = [ "ReadWriteOnce" ];
                resources.requests.storage = "500Gi";
              };

          persistentVolumes = lib.optionalAttrs cfg.nfs.enable {
            "${name}-${name}-downloads-nfs" = {
              apiVersion = "v1";
              kind = "PersistentVolume";
              metadata.name = "${name}-${name}-downloads-nfs";
              spec = {
                capacity.storage = "2Ti";
                accessModes = [ "ReadWriteMany" ];
                mountOptions = [
                  "nolock"
                  "noexec"
                  "soft"
                  "timeo=30"
                ];
                nfs = {
                  server = cfg.nfs.server;
                  path = cfg.nfs.path;
                };
                persistentVolumeReclaimPolicy = "Retain";
              };
            };
          };
        };
      }
    );
}
