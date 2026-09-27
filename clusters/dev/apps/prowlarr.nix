{ config, secrets, ... }:
{
  services.prowlarr = {
    databaseTarget = "postgresql";
    database = {
      enable = true;
      name = "prowlarr-main";
    };

    enable = true;
    apiKey = secrets.prowlarr.key;
    hostAffinity = "edgenix";
    image = "linuxserver/prowlarr:2.5.2.5491-ls156";

    ingressProvider = "traefik-lan";
    ingress.tls.enable = true;
    homepage.group = "Arr";

    replicas = 1;
    vpn.enable = false;

    # Captured via `kubectl get pv <name> -o jsonpath='{.spec.csi.volumeHandle}'`
    # -- see docs/pinned-volumes.md. Specific to this cluster.
    volumeOverrides.config.volumeHandle = "pvc-ee5907d3-b4e4-4da5-91dc-d013f243b741";
  };
}
