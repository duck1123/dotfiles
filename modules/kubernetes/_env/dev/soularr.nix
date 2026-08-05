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
  };
}
