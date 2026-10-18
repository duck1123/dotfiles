{ config, secrets, ... }:
{
  services.slskd = {
    apiKey = (secrets.slskd or { }).apiKey or "";
    enable = true;
    # hostAffinity = "edgenix";

    homepage.group = "Download";

    ingressProvider = "traefik-lan";
    ingress.tls.enable = true;

    nfs = {
      enable = true;
      server = config.devDefaults.nasHost;
      path = "${config.devDefaults.nasBase}/slskd_downloads";
    };

    replicas = 1;

    shares = {
      enable = true;
      server = config.devDefaults.nasHost;
      path = "${config.devDefaults.nasBase}/Music";
    };

    storageClassName = "longhorn";
    useProbes = false;

    vpn = {
      enable = true;
      sharedGluetunService = "gluetun.gluetun";
    };

    webAuth = {
      username = (secrets.slskd or { }).username or "";
      password = (secrets.slskd or { }).password or "";
    };

    # Captured via `kubectl get pv <name> -o jsonpath='{.spec.csi.volumeHandle}'`
    # -- see docs/pinned-volumes.md. Specific to this cluster.
    volumeOverrides.config.volumeHandle = "pvc-3daa90da-e4b2-4dd2-8cfe-9a1f98764991";
  };
}
