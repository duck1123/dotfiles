{ secrets, ... }:
{
  services.sonarr = {
    databaseTarget = "postgresql";
    database.enable = true;

    enable = true;
    apiKey = secrets.sonarr.key;
    image = "linuxserver/sonarr:4.0.19.2979-ls323";
    hostAffinity = "edgenix";

    ingressProvider = "traefik-lan";
    ingress.tls.enable = true;
    homepage.group = "Arr";

    nfsTarget = "nas";
    nfs.enable = true;

    replicas = 1;
    vpn.enable = false;

    # Captured via `kubectl get pv <name> -o jsonpath='{.spec.csi.volumeHandle}'`
    # -- see docs/pinned-volumes.md. Specific to this cluster.
    volumeOverrides.config.volumeHandle = "pvc-bda437d3-382b-4c6b-bbd1-3982593d07fb";
  };
}
