{ config, secrets, ... }:
{
  services.mealie = {
    enable = false;
    # hostAffinity = "edgenix";
    image = "ghcr.io/mealie-recipes/mealie:v3.25.1";

    databaseTarget = "postgresql";
    database = {
      enable = true;
      password = secrets.mealie.databasePassword;
    };

    ingressProvider = "traefik-lan";
    ingress.tls.enable = true;

    storageClassName = "longhorn";

    # Captured via `kubectl get pv <name> -o jsonpath='{.spec.csi.volumeHandle}'`
    # -- see docs/pinned-volumes.md. Specific to this cluster.
    volumeOverrides.data.volumeHandle = "pvc-4e3d0692-5c4d-4a65-9f62-3c0f43326ede";
  };
}
