{ ... }:
{
  flake.nixidyApps.demo =
    {
      config,
      lib,
      pkgs,
      self,
      ...
    }:
    with lib;
    let
      name = "demo";
      labels = {
        "app.kubernetes.io/name" = name;
      };

      # ── Runtime ──────────────────────────────────────────────────────────────
      # nix-csi evaluates this expression on the node and mounts the result at
      # /nix so its bin directory is on PATH inside the scratch container.
      # Swap pkgs.python3 for any other nixpkgs derivation to change the runtime.
      serverExpr = ''
        let
          pkgs = import (builtins.fetchTree {
            type = "github";
            owner = "nixos";
            repo = "nixpkgs";
            ref = "nixos-unstable";
          }) {};
        in
        pkgs.python3
      '';

      # ── Server script ─────────────────────────────────────────────────────────
      # Stored in a ConfigMap so you can change server behaviour without touching
      # the runtime expression above.  Edit freely — standard Python, no deps.
      serverScript = ''
        import http.server
        import json
        import os
        import sys

        DATA_FILE = os.environ.get("JSON_DATA_FILE", "/data/data.json")
        PORT = int(os.environ.get("PORT", "8080"))


        class Handler(http.server.BaseHTTPRequestHandler):
            def log_message(self, fmt, *args):
                sys.stderr.write(fmt % args + "\n")
                sys.stderr.flush()

            def do_GET(self):
                try:
                    with open(DATA_FILE) as f:
                        body = f.read().encode()
                except Exception as exc:
                    body = json.dumps({"error": str(exc)}).encode()

                self.send_response(200)
                self.send_header("Content-Type", "application/json")
                self.send_header("Content-Length", str(len(body)))
                self.end_headers()
                self.wfile.write(body)


        print(f"json-server listening on :{PORT}", file=sys.stderr, flush=True)
        http.server.HTTPServer(("", PORT), Handler).serve_forever()
      '';
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
        name = "demo";
        uses-ingress = true;

        extraOptions = {
          # ── Data ───────────────────────────────────────────────────────────────
          # Change this attrset (or override it in env/dev.nix) to serve different
          # JSON.  The value is serialised to /data/data.json inside the pod.
          jsonData = mkOption {
            description = mdDoc "JSON object served at GET /";
            type = types.attrs;
            default = {
              message = "Hello from nix-csi!";
              runtime = "python3";
            };
          };
        };

        extraResources =
          cfg:
          let
            port = 8080;
          in
          {
            configMaps = {
              demo-data.data."data.json" = builtins.toJSON cfg.jsonData;
              demo-server.data."server.py" = serverScript;
            };

            deployments.${name}.spec = {
              selector.matchLabels = labels;
              template = {
                metadata.labels = labels;
                spec = {
                  initContainers = [
                    {
                      name = "nix-debug";
                      image = "busybox:1.38";
                      command = [
                        "sh"
                        "-c"
                        ''
                          echo "=== /nix-csi (volume root) ===" ; ls -la /nix-csi ;
                          echo "=== /nix-csi/nix ===" ; ls -la /nix-csi/nix 2>/dev/null || echo "(no nix/)" ;
                          echo "=== /nix-csi/nix/var ===" ; ls -la /nix-csi/nix/var 2>/dev/null || echo "(no nix/var/)" ;
                          echo "=== /nix-csi/nix/var/result ===" ; ls -la /nix-csi/nix/var/result 2>/dev/null || echo "(no result symlink)" ;
                          echo "=== /nix (subPath nix) ===" ; ls -la /nix 2>/dev/null || echo "(no /nix)" ;
                          echo "=== /nix/var/result/bin ===" ; ls /nix/var/result/bin 2>/dev/null || echo "(no /nix/var/result/bin)" ;
                          echo "=== /nix/store (first 10) ===" ; ls /nix/store 2>/dev/null | head -10 || echo "(no /nix/store)"
                        ''
                      ];
                      volumeMounts = [
                        {
                          name = "nix";
                          mountPath = "/nix-csi";
                        }
                        {
                          name = "nix";
                          mountPath = "/nix";
                          subPath = "nix";
                        }
                      ];
                    }
                  ];
                  containers = [
                    {
                      inherit name;
                      image = "ghcr.io/lillecarl/nix-csi/scratch:1.0.1";
                      command = [
                        "python3"
                        "/scripts/server.py"
                      ];
                      env = [
                        {
                          name = "PORT";
                          value = toString port;
                        }
                        {
                          name = "JSON_DATA_FILE";
                          value = "/data/data.json";
                        }
                      ];
                      ports = [
                        {
                          containerPort = port;
                          name = "http";
                          protocol = "TCP";
                        }
                      ];
                      volumeMounts = [
                        {
                          name = "nix";
                          mountPath = "/nix";
                          subPath = "nix";
                        }
                        {
                          name = "scripts";
                          mountPath = "/scripts";
                        }
                        {
                          name = "data";
                          mountPath = "/data";
                        }
                      ];
                    }
                  ];
                  volumes = [
                    {
                      name = "nix";
                      csi = {
                        driver = "nix.csi.store";
                        volumeAttributes.nixExpr = serverExpr;
                      };
                    }
                    {
                      name = "scripts";
                      configMap.name = "demo-server";
                    }
                    {
                      name = "data";
                      configMap.name = "demo-data";
                    }
                  ];
                };
              };
            };

            ingresses.${name}.spec = with cfg.ingress; {
              inherit ingressClassName;
              rules = [
                {
                  host = domain;
                  http.paths = [
                    {
                      path = "/";
                      pathType = "ImplementationSpecific";
                      backend.service = {
                        inherit name;
                        port.name = "http";
                      };
                    }
                  ];
                }
              ];
              tls = [ { hosts = [ domain ]; } ];
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
      };
}
