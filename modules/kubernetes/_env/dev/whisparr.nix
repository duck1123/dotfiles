{ config, ... }:
{
  services.whisparr = {
    databaseTarget = "postgresql";
    database.enable = true;

    enable = false;

    ingressProvider = "traefik-lan";

    nfs = {
      enable = true;
      server = config.devDefaults.nasHost;
      path = "${config.devDefaults.nasBase}";
    };

    replicas = 1;

    # Captured via `kubectl get pv <name> -o jsonpath='{.spec.csi.volumeHandle}'`
    # -- see docs/pinned-volumes.md. Specific to this cluster.
    volumeOverrides.config.volumeHandle = "pvc-7635e288-2ecd-476b-9cc8-e6355eb2ee27";
  };
}
