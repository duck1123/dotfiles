{ ... }:
{
  flake.nixidyApps.sabnzbd =
    {
      config,
      lib,
      self,
      ...
    }:
    with lib;
    let
      cfg = config.services.sabnzbd;
    in
    self.lib.mkArgoApp { inherit config lib self; } rec {
      name = "sabnzbd";
      uses-ingress = true;

      # Shape only -- no volumeHandle here, that's environment-specific (see
      # env/dev/sabnzbd.nix and docs/pinned-volumes.md).
      volumes = cfg: {
        config.size = "5Gi";
      };

      extraOptions = {
        image = mkOption {
          description = mdDoc "The docker image";
          type = types.str;
          default = "linuxserver/sabnzbd:latest";
        };

        service.port = mkOption {
          description = mdDoc "The service port";
          type = types.int;
          default = 8080;
        };

        apiKey = mkOption {
          description = mdDoc ''
            SABnzbd API key (Config -> General). Stored in secrets.enc.yaml as
            `sabnzbd.key` and wired in via `env/dev/sabnzbd.nix`. Only powers
            the auto-added homepage dashboard widget below -- never injected
            into the sabnzbd container itself.
          '';
          type = types.str;
          default = "";
        };

        # Auto-add a SABnzbd widget to this app's homepage dashboard tile once
        # an API key is configured -- see applications/immich.nix for the same
        # pattern with more detail. Set
        # `services.homepage.widgetSecrets.SABNZBD_API_KEY` from
        # `config.services.sabnzbd.apiKey` in env/dev/homepage.nix.
        homepage.extraSettings = mkOption {
          default = lib.optionalAttrs (cfg.apiKey != "") {
            widget = {
              type = "sabnzbd";
              url = "http://${name}.${cfg.namespace}:${toString cfg.service.port}";
              key = "{{HOMEPAGE_VAR_SABNZBD_API_KEY}}";
            };
          };
        };

        vpn = {
          enable = mkOption {
            description = mdDoc "Enable VPN routing through shared gluetun service";
            type = types.bool;
            default = true;
          };

          sharedGluetunService = mkOption {
            description = mdDoc "Service name for shared gluetun (e.g., gluetun.gluetun)";
            type = types.str;
            default = "gluetun.gluetun";
          };
        };

        nfs = {
          enable = mkOption {
            description = mdDoc "Enable NFS for downloads volume";
            type = types.bool;
            default = false;
          };

          server = mkOption {
            description = mdDoc "NFS server hostname/IP";
            type = types.str;
            default = "nasnix";
          };

          path = mkOption {
            description = mdDoc "NFS server path";
            type = types.str;
            default = "/mnt/media";
          };
        };

        pgid = mkOption {
          description = mdDoc "The group ID";
          type = types.int;
          default = 1000;
        };

        puid = mkOption {
          description = mdDoc "The user ID";
          type = types.int;
          default = 1000;
        };

        hostWhitelist = mkOption {
          description = mdDoc "Comma-separated list of hostnames allowed to access SABnzbd. The service name and FQDN are automatically added.";
          type = types.str;
          default = "";
        };

        debugLogging = mkOption {
          description = mdDoc "Enable debug logging in SABnzbd (log_level = 2)";
          type = types.bool;
          default = false;
        };

        replicas = mkOption {
          description = mdDoc "Number of replicas";
          type = types.int;
          default = 1;
        };

        useProbes = mkOption {
          description = mdDoc "Enable readiness and liveness probes";
          type = types.bool;
          default = true;
        };
      };

      extraResources = cfg: {
        deployments.${name} = {
          metadata.labels = {
            "app.kubernetes.io/instance" = name;
            "app.kubernetes.io/name" = name;
            "app.kubernetes.io/version" = "latest";
          };

          spec = {
            replicas = cfg.replicas;
            strategy.type = "Recreate";
            selector.matchLabels = {
              "app.kubernetes.io/instance" = name;
              "app.kubernetes.io/name" = name;
            };

            template = {
              metadata.labels = {
                "app.kubernetes.io/instance" = name;
                "app.kubernetes.io/name" = name;
              };

              spec = {
                automountServiceAccountToken = true;
                serviceAccountName = "default";

                # Init container to set hostname whitelist in config
                initContainers =
                  let
                    hostnameList = lib.concatStringsSep "," (
                      lib.filter (x: x != "") [
                        cfg.hostWhitelist
                        (if cfg.ingress.domain != "" then cfg.ingress.domain else "")
                        "${name}.${cfg.namespace}"
                        "${name}.${cfg.namespace}.svc.cluster.local"
                      ]
                    );
                  in
                  [
                    {
                      name = "set-hostname-whitelist";
                      image = "busybox:latest";
                      command = [
                        "sh"
                        "-c"
                        ''
                          CONFIG_FILE="/config/sabnzbd.ini"
                          ALL_HOSTNAMES="${hostnameList}"

                          # Clean up duplicates (split by comma, remove empty, sort, rejoin with comma)
                          ALL_HOSTNAMES=$(echo "$ALL_HOSTNAMES" | tr ',' '\n' | grep -v '^$' | sort -u | tr '\n' ',' | sed 's/,$//')

                          # Wait for config volume to be available
                          while [ ! -d /config ]; do
                            sleep 1
                          done

                          # Create config file if it doesn't exist
                          if [ ! -f "$CONFIG_FILE" ]; then
                            touch "$CONFIG_FILE"
                          fi

                          # Ensure [misc] section exists
                          if ! grep -q "^\[misc\]" "$CONFIG_FILE"; then
                            echo "" >> "$CONFIG_FILE"
                            echo "[misc]" >> "$CONFIG_FILE"
                          fi

                          # Remove any existing host_whitelist entry
                          sed -i '/^host_whitelist/d' "$CONFIG_FILE"

                          # Add host_whitelist to [misc] section
                          if grep -q "^\[misc\]" "$CONFIG_FILE"; then
                            sed -i "/^\[misc\]/a host_whitelist = $ALL_HOSTNAMES" "$CONFIG_FILE"
                          else
                            echo "host_whitelist = $ALL_HOSTNAMES" >> "$CONFIG_FILE"
                          fi
                          echo "Hostname whitelist set to: $ALL_HOSTNAMES"

                          ${lib.optionalString cfg.debugLogging ''
                            # Enable debug logging if requested
                            if ! grep -q "^\[logging\]" "$CONFIG_FILE"; then
                              echo "" >> "$CONFIG_FILE"
                              echo "[logging]" >> "$CONFIG_FILE"
                            fi

                            sed -i '/^log_level/d' "$CONFIG_FILE"
                            sed -i '/^log_backups/d' "$CONFIG_FILE"
                            sed -i '/^max_log_size/d' "$CONFIG_FILE"
                            sed -i '/^log_access/d' "$CONFIG_FILE"
                            if grep -q "^\[logging\]" "$CONFIG_FILE"; then
                              sed -i "/^\[logging\]/a log_level = 2" "$CONFIG_FILE"
                              sed -i "/^\[logging\]/a log_backups = 5" "$CONFIG_FILE"
                              sed -i "/^\[logging\]/a max_log_size = 10M" "$CONFIG_FILE"
                              sed -i "/^\[logging\]/a log_access = 1" "$CONFIG_FILE"
                            else
                              echo "log_level = 2" >> "$CONFIG_FILE"
                              echo "log_backups = 5" >> "$CONFIG_FILE"
                              echo "max_log_size = 10M" >> "$CONFIG_FILE"
                              echo "log_access = 1" >> "$CONFIG_FILE"
                            fi
                            echo "Debug logging enabled (log_level = 2, log_access = 1)"
                          ''}
                        ''
                      ];
                      volumeMounts = [
                        {
                          mountPath = "/config";
                          name = "config";
                        }
                      ];
                    }
                  ];
                containers = [
                  {
                    inherit name;
                    image = cfg.image;
                    imagePullPolicy = "IfNotPresent";
                    env = [
                      {
                        name = "PGID";
                        value = "${toString cfg.pgid}";
                      }
                      {
                        name = "PUID";
                        value = "${toString cfg.puid}";
                      }
                      {
                        name = "TZ";
                        value = cfg.tz;
                      }
                      {
                        name = "PYTHONUNBUFFERED";
                        value = "1";
                      }
                    ]
                    ++ (lib.optionalAttrs cfg.vpn.enable [
                      # Configure Sabnzbd to use shared gluetun's HTTP proxy
                      {
                        name = "HTTP_PROXY";
                        value = "http://${cfg.vpn.sharedGluetunService}:8888";
                      }
                      {
                        name = "HTTPS_PROXY";
                        value = "http://${cfg.vpn.sharedGluetunService}:8888";
                      }
                      {
                        name = "NO_PROXY";
                        value = "localhost,127.0.0.1,.svc,.svc.cluster.local,${name}.${cfg.namespace},${name}.${cfg.namespace}.svc.cluster.local";
                      }
                    ]);
                    ports = [
                      {
                        containerPort = cfg.service.port;
                        name = "http";
                        protocol = "TCP";
                      }
                    ];
                    readinessProbe = lib.mkIf cfg.useProbes {
                      httpGet = {
                        path = "/api?mode=version";
                        port = cfg.service.port;
                      };
                      initialDelaySeconds = 120;
                      periodSeconds = 15;
                      timeoutSeconds = 10;
                      successThreshold = 1;
                      failureThreshold = 30;
                    };
                    livenessProbe = lib.mkIf cfg.useProbes {
                      httpGet = {
                        path = "/api?mode=version";
                        port = cfg.service.port;
                      };
                      initialDelaySeconds = 180;
                      periodSeconds = 30;
                      timeoutSeconds = 10;
                      successThreshold = 1;
                      failureThreshold = 20;
                    };

                    volumeMounts = [
                      {
                        mountPath = "/config";
                        name = "config";
                      }
                      {
                        mountPath = "/config/Downloads";
                        name = "downloads";
                      }
                      {
                        mountPath = "/config/incomplete-downloads";
                        name = "incomplete-downloads";
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
                  {
                    name = "incomplete-downloads";
                    persistentVolumeClaim.claimName = "${name}-${name}-incomplete-downloads";
                  }
                ];
              };
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

        persistentVolumeClaims = {
          "${name}-${name}-downloads".spec =
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
                resources.requests.storage = "50Gi";
              };
          "${name}-${name}-incomplete-downloads".spec =
            if cfg.nfs.enable then
              {
                accessModes = [ "ReadWriteMany" ];
                resources.requests.storage = "1Gi";
                storageClassName = "";
                volumeName = "${name}-${name}-incomplete-downloads-nfs";
              }
            else
              {
                inherit (cfg) storageClassName;
                accessModes = [ "ReadWriteOnce" ];
                resources.requests.storage = "20Gi";
              };
        };

        services.${name}.spec = {
          ports = [
            {
              name = "http";
              port = cfg.service.port;
              protocol = "TCP";
              targetPort = "http";
            }
          ];

          selector = {
            "app.kubernetes.io/instance" = name;
            "app.kubernetes.io/name" = name;
          };

          type = "ClusterIP";
        };

        # Create NFS PersistentVolumes for downloads when NFS is enabled
        persistentVolumes = lib.optionalAttrs (cfg.nfs.enable) {
          "${name}-${name}-downloads-nfs" = {
            metadata.name = "${name}-${name}-downloads-nfs";
            spec = {
              accessModes = [ "ReadWriteMany" ];
              capacity.storage = "1Ti";
              mountOptions = [
                "nolock"
                "noexec"
                "soft"
                "timeo=30"
              ];
              nfs = {
                server = cfg.nfs.server;
                path = "${cfg.nfs.path}/Downloads";
              };
              persistentVolumeReclaimPolicy = "Retain";
            };
          };
          "${name}-${name}-incomplete-downloads-nfs" = {
            metadata.name = "${name}-${name}-incomplete-downloads-nfs";
            spec = {
              accessModes = [ "ReadWriteMany" ];
              capacity.storage = "1Ti";
              mountOptions = [
                "nolock"
                "noexec"
                "soft"
                "timeo=30"
              ];
              nfs = {
                server = cfg.nfs.server;
                path = "${cfg.nfs.path}/Downloads/incomplete";
              };
              persistentVolumeReclaimPolicy = "Retain";
            };
          };
        };
      };
    };
}
