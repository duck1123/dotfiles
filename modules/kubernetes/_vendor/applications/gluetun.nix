{ ... }:
{
  flake.nixidyApps.gluetun =
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
      rec {
        name = "gluetun";
        uses-ingress = false;

        # Shape only -- no volumeHandle here, that's environment-specific (see
        # env/dev/gluetun.nix and docs/pinned-volumes.md).
        volumes = cfg: {
          gluetun.size = "1Gi";
        };

        sopsSecrets =
          cfg:
          {
            "${name}-mullvad-wireguard" = {
              privateKey = cfg.wireguardPrivateKey;
            };
          }
          // lib.optionalAttrs (cfg.controlServer.username != "" || cfg.controlServer.password != "") {
            "${name}-control-server" =
              let
                authHeader =
                  "Basic "
                  + (builtins.readFile (
                    pkgs.runCommand "gluetun-auth-header" { } ''
                      echo -n "${cfg.controlServer.username}:${cfg.controlServer.password}" | ${pkgs.coreutils}/bin/base64 > $out
                    ''
                  ));
              in
              {
                username = cfg.controlServer.username;
                password = cfg.controlServer.password;
                authHeader = authHeader;
              };
          };

        extraOptions = {
          wireguardPrivateKey = mkOption {
            description = mdDoc "WireGuard private key for Mullvad (generate at mullvad.net/en/account/wireguard-config)";
            type = types.str;
            default = "";
          };

          wireguardAddresses = mkOption {
            description = mdDoc "Comma-separated WireGuard addresses assigned to your key (e.g. 10.64.x.x/32,fc00:bbbb::.../128)";
            type = types.str;
            default = "";
          };

          serverLocation = mkOption {
            description = mdDoc "Mullvad server location (e.g., us-was, se-sto)";
            type = types.str;
            default = "";
          };

          serverCountry = mkOption {
            description = mdDoc "Mullvad server country (e.g., USA, Sweden)";
            type = types.nullOr types.str;
            default = null;
          };

          enableIPv6 = mkOption {
            description = mdDoc ''
              Enable IPv6 support. This requires:
              1. Kubernetes cluster with dual-stack networking enabled
              2. CNI plugin that supports IPv6 (e.g., Cilium, Calico with IPv6)
              3. IPv6 addresses assigned to nodes
              4. IPv6 routing configured in the cluster

              If IPv6 is not properly configured in your cluster, you'll see
              "Network unreachable" errors. In that case, set this to false.
            '';
            type = types.bool;
            default = false;
          };

          logLevel = mkOption {
            description = mdDoc "Log level (debug, info, warning, error). Use 'debug' for maximum verbosity.";
            type = types.str;
            default = "info";
          };

          controlServer = {
            username = mkOption {
              description = mdDoc "HTTP control server username";
              type = types.str;
              default = "";
            };

            password = mkOption {
              description = mdDoc "HTTP control server password";
              type = types.str;
              default = "";
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

                    containers = [
                      {
                        inherit name;
                        image = "qmcgaw/gluetun:latest";
                        imagePullPolicy = "IfNotPresent";
                        securityContext = {
                          capabilities.add = [
                            "NET_ADMIN"
                            "MKNOD"
                            "NET_RAW"
                          ];
                          privileged = false;
                        };
                        env = lib.filter (x: x != null) [
                          {
                            name = "VPN_SERVICE_PROVIDER";
                            value = "mullvad";
                          }
                          {
                            name = "VPN_TYPE";
                            value = "wireguard";
                          }
                          {
                            name = "WIREGUARD_PRIVATE_KEY";
                            valueFrom.secretKeyRef = {
                              name = "${name}-mullvad-wireguard";
                              key = "privateKey";
                            };
                          }
                          {
                            name = "WIREGUARD_ADDRESSES";
                            value = cfg.wireguardAddresses;
                          }
                          (
                            if cfg.serverCountry != null && cfg.serverCountry != "" then
                              {
                                name = "SERVER_COUNTRIES";
                                value = cfg.serverCountry;
                              }
                            else
                              null
                          )
                          (
                            if cfg.serverLocation != "" then
                              {
                                name = "SERVER_CITIES";
                                value = cfg.serverLocation;
                              }
                            else
                              null
                          )
                          {
                            name = "FIREWALL";
                            value = "on";
                          }
                          {
                            name = "FIREWALL_VPN_INPUT_PORTS";
                            value = "8888,8000";
                          }
                          {
                            name = "LOG_LEVEL";
                            value = cfg.logLevel;
                          }
                          {
                            name = "HTTPPROXY_LOG";
                            value = "on";
                          }
                          {
                            name = "UPDATER_PERIOD";
                            value = "24h";
                          }
                          {
                            name = "TZ";
                            value = cfg.tz;
                          }
                          {
                            name = "HTTP_CONTROL_SERVER_LOG";
                            value = "on";
                          }
                          {
                            name = "HTTP_CONTROL_SERVER";
                            value = "on";
                          }
                          {
                            name = "DNS_KEEP_NAMESERVER";
                            value = "off";
                          }
                          {
                            name = "DNS_ADDRESS";
                            value = if cfg.enableIPv6 then "::" else "";
                          }
                          {
                            name = "DNS_IPV6";
                            value = if cfg.enableIPv6 then "on" else "off";
                          }
                          {
                            name = "DNS_UPSTREAM_IPV6";
                            value = if cfg.enableIPv6 then "on" else "off";
                          }
                          {
                            name = "HTTPPROXY";
                            value = "on";
                          }
                          {
                            name = "HTTPPROXY_LISTENING_ADDRESS";
                            value = "0.0.0.0:8888";
                          }
                          {
                            name = "HTTP_CONTROL_SERVER_LISTENING_ADDRESS";
                            value = "0.0.0.0:8000";
                          }
                          {
                            name = "HEALTH_SERVER_ADDRESS";
                            value = ":9999";
                          }
                          (
                            if cfg.controlServer.username != "" then
                              {
                                name = "HTTP_CONTROL_SERVER_USER";
                                valueFrom.secretKeyRef = {
                                  name = "${name}-control-server";
                                  key = "username";
                                };
                              }
                            else
                              null
                          )
                          (
                            if cfg.controlServer.password != "" then
                              {
                                name = "HTTP_CONTROL_SERVER_PASSWORD";
                                valueFrom.secretKeyRef = {
                                  name = "${name}-control-server";
                                  key = "password";
                                };
                              }
                            else
                              null
                          )
                        ];
                        ports = [
                          {
                            containerPort = 8888;
                            name = "http-proxy";
                            protocol = "TCP";
                          }
                          {
                            containerPort = 1080;
                            name = "socks-proxy";
                            protocol = "TCP";
                          }
                          {
                            containerPort = 8000;
                            name = "http-control";
                            protocol = "TCP";
                          }
                        ];
                        readinessProbe = {
                          httpGet = {
                            path = "/";
                            port = 9999;
                          };
                          initialDelaySeconds = 15;
                          periodSeconds = 10;
                          timeoutSeconds = 5;
                          successThreshold = 1;
                          failureThreshold = 3;
                        };
                        startupProbe = {
                          httpGet = {
                            path = "/";
                            port = 9999;
                          };
                          initialDelaySeconds = 5;
                          periodSeconds = 5;
                          timeoutSeconds = 3;
                          successThreshold = 1;
                          failureThreshold = 30;
                        };
                        livenessProbe = {
                          tcpSocket = {
                            port = 8000;
                          };
                          initialDelaySeconds = 30;
                          periodSeconds = 30;
                          timeoutSeconds = 5;
                          successThreshold = 1;
                          failureThreshold = 3;
                        };
                        volumeMounts = [
                          {
                            mountPath = "/gluetun";
                            name = "gluetun";
                          }
                        ];
                      }
                    ];

                    dnsConfig = {
                      # Use gluetun's internal DNS server (listening on port 53)
                      # This ensures DNS queries go through the VPN and aren't blocked by firewall
                      nameservers = [ "127.0.0.1" ];
                      searches = [ ];
                      options = [
                        {
                          name = "ndots";
                          value = "2";
                        }
                        {
                          name = "edns0";
                        }
                      ];
                    };

                    dnsPolicy = "None";

                    serviceAccountName = "default";

                    volumes = [ cfg.volumes.gluetun.volume ];
                  };
                };
              };
            };
          };

          services.${name}.spec = {
            ports = [
              {
                name = "http-proxy";
                port = 8888;
                protocol = "TCP";
                targetPort = "http-proxy";
              }
              {
                name = "socks-proxy";
                port = 1080;
                protocol = "TCP";
                targetPort = "socks-proxy";
              }
              {
                name = "http-control";
                port = 8000;
                protocol = "TCP";
                targetPort = "http-control";
              }
            ];

            selector = {
              "app.kubernetes.io/instance" = name;
              "app.kubernetes.io/name" = name;
            };

            type = "ClusterIP";
            # Enable dual-stack if IPv6 is enabled and cluster supports it
            ipFamilyPolicy = if cfg.enableIPv6 then "RequireDualStack" else null;
            ipFamilies =
              if cfg.enableIPv6 then
                [
                  "IPv4"
                  "IPv6"
                ]
              else
                [ "IPv4" ];
          };
        };
      };
}
