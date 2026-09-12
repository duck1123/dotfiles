{ config, secrets, ... }:
{
  services.radarr = {
    databaseTarget = "postgresql";
    database.enable = true;

    enable = true;
    apiKey = secrets.radarr.key;
    hostAffinity = "edgenix";
    image = "linuxserver/radarr:6.3.0.10514-ls315";

    ingressProvider = "traefik-lan";
    ingress.tls.enable = true;
    homepage.group = "Arr";

    nfs = {
      enable = true;
      server = config.devDefaults.nasHost;
      path = "${config.devDefaults.nasBase}";
    };

    replicas = 1;
    storageClassName = "longhorn";
    vpn.enable = false;

    # Captured via `kubectl get pv <name> -o jsonpath='{.spec.csi.volumeHandle}'`
    # -- see docs/pinned-volumes.md. Specific to this cluster.
    volumeOverrides.config.volumeHandle = "pvc-979d8477-4390-4d8e-b559-7839008e080b";
  };
}
