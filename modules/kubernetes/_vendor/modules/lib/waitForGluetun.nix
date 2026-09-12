{ ... }:
{
  flake.lib.waitForGluetun =
    { ... }:
    # Creates an init container that waits for gluetun to be ready.
    # Uses the proxy (8888) rather than the auth-protected control API (8000).
    # gluetunService: service name, e.g. "gluetun.gluetun"
    gluetunService: [
      {
        name = "wait-for-gluetun";
        image = "curlimages/curl:latest";
        command = [ "sh" ];
        args = [
          "-c"
          ''
            echo "Waiting for gluetun VPN proxy..."
            until curl -sf --connect-timeout 5 --max-time 15 \
                --proxy http://${gluetunService}:8888 \
                "https://1.1.1.1/cdn-cgi/trace" > /dev/null 2>&1; do
              echo "Proxy not ready, retrying in 5s..."
              sleep 5
            done
            echo "Gluetun proxy ready"
          ''
        ];
      }
    ];

}
