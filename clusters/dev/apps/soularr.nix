{ config, secrets, ... }:
{
  services.soularr = {
    enable = true;
    # hostAffinity = "edgenix";

    lidarr = {
      host = "lidarr.lidarr";
      port = 8686;
      downloadDir = "/downloads/slskd_downloads";
      apiKey = (secrets.soularr or { }).lidarrApiKey or "";
    };

    slskd = {
      host = "slskd.slskd";
      port = 5030;
      apiKey = (secrets.slskd or { }).apiKey or "";
    };

    nfs = {
      enable = true;
      server = config.devDefaults.nasHost;
      path = "${config.devDefaults.nasBase}/slskd_downloads";
    };

    scriptInterval = 300;
    storageClassName = "longhorn";

    # Captured via `kubectl get pv <name> -o jsonpath='{.spec.csi.volumeHandle}'`
    # -- see docs/pinned-volumes.md. Specific to this cluster.
    volumeOverrides.config.volumeHandle = "pvc-25589b0e-1276-441e-a91a-2e4e78ec378c";
  };
}
