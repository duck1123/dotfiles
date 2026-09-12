{ ... }:
{
  flake.nixidyApps.qbittorrent =
    {
      config,
      lib,
      pkgs,
      self,
      ...
    }:
    with lib;
    let
      password-secret = "qbittorrent-webui-credentials";
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
      rec {
        name = "qbittorrent";
        uses-ingress = true;

        # Shape only -- no volumeHandle here, that's environment-specific (see
        # env/dev/qbittorrent.nix and docs/pinned-volumes.md).
        volumes = cfg: {
          config.size = "5Gi";
        };

        sopsSecrets =
          cfg:
          lib.optionalAttrs (cfg.webui.username != "" && cfg.webui.password != "") {
            ${password-secret} = {
              username = cfg.webui.username;
              password = cfg.webui.password;
            };
          };

        extraOptions = {
          image = mkOption {
            description = mdDoc "The docker image";
            type = types.str;
            default = "linuxserver/qbittorrent:latest";
          };

          service.port = mkOption {
            description = mdDoc "The service port";
            type = types.int;
            default = 8080;
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

          webui = {
            username = mkOption {
              description = mdDoc "Web UI username (will be stored in SOPS secret)";
              type = types.str;
              default = "";
            };

            password = mkOption {
              description = mdDoc "Web UI password (will be stored in SOPS secret)";
              type = types.str;
              default = "";
            };

            disableAuthentication = mkOption {
              description = mdDoc "Disable Web UI authentication (not recommended)";
              type = types.bool;
              default = false;
            };
          };
        };

        extraResources = cfg: {
          deployments = {
            ${name} = {
              metadata.labels = {
                "app.kubernetes.io/instance" = name;
                "app.kubernetes.io/name" = name;
                "app.kubernetes.io/version" = "latest";
              };

              spec = {
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
                    initContainers =
                      (lib.optionals (cfg.webui.username != "" && cfg.webui.password != "") [
                        {
                          name = "configure-auth";
                          image = "python:3-alpine";
                          command = [
                            "sh"
                            "-c"
                            ''
                                                    mkdir -p /config/qBittorrent
                                                    CONFIG_FILE="/config/qBittorrent/qBittorrent.conf"

                                                    # Generate PBKDF2 password hash for qBittorrent
                                                    # qBittorrent stores PBKDF2 as: PBKDF2:sha512:100000:<salt_base64>:<hash_base64>
                                                    # But in @ByteArray() it might need hex format
                                                    # Write Python script to file
                                                    printf '%s\n' \
                                                      'import hashlib' \
                                                      'import base64' \
                                                      'import os' \
                                                      'import sys' \
                                                      'password = os.environ.get("PASSWORD", "")' \
                                                      'if not password:' \
                                                      '    print("ERROR: PASSWORD not set", file=sys.stderr)' \
                                                      '    sys.exit(1)' \
                                                      'import hashlib as hl' \
                                                      'salt = hl.sha256(password.encode("utf-8")).digest()[:16]' \
                                                      'iterations = 100000' \
                                                      'dk = hashlib.pbkdf2_hmac("sha512", password.encode("utf-8"), salt, iterations)' \
                                                      'hash_b64 = base64.b64encode(dk).decode("ascii")' \
                                                      'salt_b64 = base64.b64encode(salt).decode("ascii")' \
                                                      'pbkdf2_hash = f"PBKDF2:sha512:{iterations}:{salt_b64}:{hash_b64}"' \
                                                      'pbkdf2_salt_hash = f"{salt_b64}:{hash_b64}"' \
                                                      'sha1_hash = hashlib.sha1(password.encode("utf-8")).hexdigest()' \
                                                      'print(f"export PBKDF2_HASH={chr(39)}{pbkdf2_hash}{chr(39)}")' \
                                                      'print(f"export PBKDF2_SALT_HASH={chr(39)}{pbkdf2_salt_hash}{chr(39)}")' \
                                                      'print(f"export SHA1_HASH={chr(39)}{sha1_hash}{chr(39)}")' \
                                                      > /tmp/gen_hash.py
                                                    if ! eval $(python3 /tmp/gen_hash.py); then
                                                      echo "ERROR: Failed to generate password hash" >&2
                                                      exit 1
                                                    fi
                                                    echo "Generated password hashes successfully"

                                                    # Ensure [Preferences] section exists
                                                    if ! grep -q "^\[Preferences\]" "$CONFIG_FILE" 2>/dev/null; then
                                                      echo "[Preferences]" >> "$CONFIG_FILE"
                                                    fi

                                                    # Always regenerate password from secret to ensure correct format
                                                    # Remove all existing WebUI settings to avoid conflicts
                                                    sed -i '/^WebUI\\/d' "$CONFIG_FILE" 2>/dev/null || true

                                                    # Add all required WebUI settings to [Preferences] section
                                                    # Find the line number of [Preferences] and insert after it
                                                    PREF_LINE=$(grep -n "^\[Preferences\]" "$CONFIG_FILE" | cut -d: -f1)
                                                    if [ -n "$PREF_LINE" ]; then
                                                      # Insert settings after [Preferences] line using sed
                                                      {
                                                        head -n "$PREF_LINE" "$CONFIG_FILE"
                                                        echo "WebUI\\Enabled=true"
                                                        echo "WebUI\\Address=0.0.0.0"
                                                        echo "WebUI\\Port=8080"
                                                        echo "WebUI\\LocalHostAuth=false"
                                                        echo "WebUI\\AuthSubnetWhitelist=@Invalid()"
                                                        echo "WebUI\\Username=$USERNAME"
                                                        echo "WebUI\\Password_ha1=@ByteArray($SHA1_HASH)"
                                                        echo "WebUI\\Password_PBKDF2=@ByteArray($PBKDF2_SALT_HASH)"
                                                        echo "WebUI\\HostHeaderValidation=false"
                                                        echo "WebUI\\CSRFProtection=false"
                                                        echo "WebUI\\ClickjackingProtection=false"
                                                        echo "WebUI\\ServerDomains=$''${WEBUI_DOMAIN:-*}"
                                                        echo "WebUI\\MaxAuthenticationFailCount=0"
                                                        echo "WebUI\\BanDuration=3600"
                                                        tail -n +$((PREF_LINE + 1)) "$CONFIG_FILE"
                                                      } > "$CONFIG_FILE.tmp" && mv "$CONFIG_FILE.tmp" "$CONFIG_FILE"
                                                    else
                                                      # If [Preferences] not found, append to end
                                                      cat >> "$CONFIG_FILE" <<EOF

                              [Preferences]
                              WebUI\Enabled=true
                              WebUI\Address=0.0.0.0
                              WebUI\Port=8080
                              WebUI\LocalHostAuth=false
                              WebUI\AuthSubnetWhitelist=@Invalid()
                              WebUI\Username=$USERNAME
                              EOF
                                                      cat >> "$CONFIG_FILE" <<PASS_EOF
                              WebUI\Password_ha1=@ByteArray($SHA1_HASH)
                              WebUI\Password_PBKDF2=@ByteArray($PBKDF2_SALT_HASH)
                              PASS_EOF
                                                      cat >> "$CONFIG_FILE" <<EOF
                              WebUI\HostHeaderValidation=false
                              WebUI\CSRFProtection=false
                              WebUI\ClickjackingProtection=false
                              WebUI\ServerDomains=$''${WEBUI_DOMAIN:-*}
                              WebUI\MaxAuthenticationFailCount=0
                              WebUI\BanDuration=3600
                              EOF
                                                    fi

                                                    # Verify critical settings were added
                                                    if ! grep -q "^WebUI\\\\Enabled=true" "$CONFIG_FILE"; then
                                                      echo "ERROR: WebUI\\Enabled not set!" >&2
                                                      exit 1
                                                    fi
                                                    if ! grep -q "^WebUI\\\\LocalHostAuth=false" "$CONFIG_FILE"; then
                                                      echo "ERROR: WebUI\\LocalHostAuth not set correctly!" >&2
                                                      exit 1
                                                    fi
                                                    echo "WebUI configuration updated successfully"
                            ''
                          ];
                          volumeMounts = [
                            {
                              mountPath = "/config";
                              name = "config";
                            }
                          ];
                          env = [
                            {
                              name = "USERNAME";
                              valueFrom = {
                                secretKeyRef = {
                                  name = password-secret;
                                  key = "username";
                                };
                              };
                            }
                            {
                              name = "PASSWORD";
                              valueFrom = {
                                secretKeyRef = {
                                  name = password-secret;
                                  key = "password";
                                };
                              };
                            }
                            {
                              name = "WEBUI_DOMAIN";
                              value = cfg.ingress.domain;
                            }
                          ];
                        }
                      ])
                      ++ (lib.optionals cfg.webui.disableAuthentication [
                        {
                          name = "disable-auth";
                          image = "busybox:latest";
                          command = [
                            "sh"
                            "-c"
                            ''
                                                  mkdir -p /config/qBittorrent
                                                  CONFIG_FILE="/config/qBittorrent/qBittorrent.conf"

                                                  if [ ! -f "$CONFIG_FILE" ]; then
                                                    cat > "$CONFIG_FILE" <<'EOF'
                              [Preferences]
                              WebUI\Enabled=true
                              WebUI\Address=*
                              WebUI\Port=8080
                              WebUI\LocalHostAuth=false
                              WebUI\AuthSubnetWhitelist=
                              WebUI\HostHeaderValidation=false
                              WebUI\CSRFProtection=false
                              WebUI\ClickjackingProtection=false
                              EOF
                                                  else
                                                    sed -i 's/^WebUI\\LocalHostAuth=.*/WebUI\\LocalHostAuth=false/' "$CONFIG_FILE" || true
                                                    sed -i 's/^WebUI\\HostHeaderValidation=.*/WebUI\\HostHeaderValidation=false/' "$CONFIG_FILE" || true
                                                    grep -q "WebUI\\\\LocalHostAuth" "$CONFIG_FILE" || echo "WebUI\\LocalHostAuth=false" >> "$CONFIG_FILE"
                                                    grep -q "WebUI\\\\HostHeaderValidation" "$CONFIG_FILE" || echo "WebUI\\HostHeaderValidation=false" >> "$CONFIG_FILE"
                                                  fi
                            ''
                          ];
                          volumeMounts = [
                            {
                              mountPath = "/config";
                              name = "config";
                            }
                          ];
                        }
                      ])
                      ++ [
                        {
                          name = "configure-downloads-path";
                          image = "busybox:latest";
                          command = [
                            "sh"
                            "-c"
                            ''
                              mkdir -p /config/qBittorrent
                              CONFIG_FILE="/config/qBittorrent/qBittorrent.conf"

                              if [ ! -f "$CONFIG_FILE" ]; then
                                cat > "$CONFIG_FILE" <<'EOF'
                              [Preferences]
                              Downloads\SavePath=/downloads/complete/
                              Downloads\TempPath=/downloads/incomplete/
                              Downloads\TempPathEnabled=true
                              EOF
                              else
                                grep -q '^\[Preferences\]' "$CONFIG_FILE" || echo '[Preferences]' >> "$CONFIG_FILE"
                                sed -i 's#^Downloads\\SavePath=.*#Downloads\\SavePath=/downloads/complete/#' "$CONFIG_FILE"
                                grep -q '^Downloads\\SavePath=' "$CONFIG_FILE" || echo 'Downloads\SavePath=/downloads/complete/' >> "$CONFIG_FILE"
                                sed -i 's#^Downloads\\TempPath=.*#Downloads\\TempPath=/downloads/incomplete/#' "$CONFIG_FILE"
                                grep -q '^Downloads\\TempPath=' "$CONFIG_FILE" || echo 'Downloads\TempPath=/downloads/incomplete/' >> "$CONFIG_FILE"
                                sed -i 's#^Downloads\\TempPathEnabled=.*#Downloads\\TempPathEnabled=true#' "$CONFIG_FILE"
                                grep -q '^Downloads\\TempPathEnabled=' "$CONFIG_FILE" || echo 'Downloads\TempPathEnabled=true' >> "$CONFIG_FILE"
                              fi
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
                        ]
                        ++ (lib.optionalAttrs cfg.vpn.enable [
                          # Configure qBittorrent to use shared gluetun's HTTP proxy
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
                            value = "localhost,127.0.0.1,.svc,.svc.cluster.local";
                          }
                        ]);
                        ports = [
                          {
                            containerPort = cfg.service.port;
                            name = "http";
                            protocol = "TCP";
                          }
                          {
                            containerPort = 6881;
                            name = "torrent-tcp";
                            protocol = "TCP";
                          }
                          {
                            containerPort = 6881;
                            name = "torrent-udp";
                            protocol = "UDP";
                          }
                        ];
                        volumeMounts = [
                          {
                            mountPath = "/config";
                            name = "config";
                          }
                          {
                            mountPath = "/downloads";
                            name = "downloads";
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
            };
          };

          ingresses.${name} = with cfg.ingress; {
            metadata.annotations."cert-manager.io/cluster-issuer" = clusterIssuer;
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
                  secretName = "${name}-tls";
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
                  resources.requests.storage = "100Gi";
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
              {
                name = "torrent-tcp";
                port = 6881;
                protocol = "TCP";
                targetPort = "torrent-tcp";
              }
              {
                name = "torrent-udp";
                port = 6881;
                protocol = "UDP";
                targetPort = "torrent-udp";
              }
            ];

            selector = {
              "app.kubernetes.io/instance" = name;
              "app.kubernetes.io/name" = name;
            };

            type = "ClusterIP";
          };

          # Create NFS PersistentVolume for downloads when NFS is enabled
          persistentVolumes = lib.optionalAttrs (cfg.nfs.enable) {
            "${name}-${name}-downloads-nfs" = {
              apiVersion = "v1";
              kind = "PersistentVolume";
              metadata = {
                name = "${name}-${name}-downloads-nfs";
              };
              spec = {
                capacity = {
                  storage = "1Ti";
                };
                accessModes = [ "ReadWriteMany" ];
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
          };
        };
      };
}
