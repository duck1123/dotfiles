{ config, secrets, ... }:
{
  services.qbittorrent = {
    enable = true;
    hostAffinity = "nasnix";

    homepage.group = "Download";

    ingressProvider = "traefik-lan";

    nfs = {
      enable = true;
      server = config.devDefaults.nasHost;
      path = "${config.devDefaults.nasBase}";
    };

    webui = { inherit (secrets.qbittorrent) password username; };

    # Captured via `kubectl get pv <name> -o jsonpath='{.spec.csi.volumeHandle}'`
    # -- see docs/pinned-volumes.md. Specific to this cluster.
    volumeOverrides.config.volumeHandle = "pvc-bc3fea68-9795-4b43-87f3-49f7a7ebd41c";
  };
}
