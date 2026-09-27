{ config, ... }:
{
  services.home-assistant = {
    enable = false;
    # hostAffinity = "edgenix";

    # https://github.com/AiDot-Development-Team/hass-AiDot
    installAidot.enable = true;

    ingressProvider = "traefik-lan";
    ingress.tls.enable = true;

    monitoring.autokuma.enable = true;
    storageClassName = "longhorn";

    # Captured via `kubectl get pv <name> -o jsonpath='{.spec.csi.volumeHandle}'`
    # -- see docs/pinned-volumes.md. Specific to this cluster.
    volumeOverrides.config.volumeHandle = "pvc-fbf41b36-718b-4942-bbe1-adf65e5bc7d1";
  };
}
