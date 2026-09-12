{ config, secrets, ... }:
{
  services.rustfs = {
    accessKey = (secrets.rustfs or { }).accessKey or "";
    enable = false;
    hostAffinity = "nasnix";

    ingressProvider = "traefik-lan";
    ingress = {
      api-domain = "api-rustfs.${config.devDefaults.homeDomain}";
      tls.enable = true;
    };

    # "/" requires auth and correctly 403s when healthy -- not a usable
    # unauthenticated health check. "/health" returns a plain 200 on both
    # the console and S3 API ports without needing credentials.
    monitoring.autokuma = {
      enable = true;
      url = "https://rustfs.${config.devDefaults.homeDomain}/health";
    };

    mode = "standalone";

    # Matches duck's NAS account uid/gid so NFS writes are actually authorized
    # server-side (the NFS export's "no mapping" squash passes the client uid
    # through as-is — it doesn't grant access on its own).
    uid = 1000;

    nfs = {
      enable = true;
      server = config.devDefaults.nasHost;
      path = "${config.devDefaults.nasBase}/RustFS";
    };

    secretKey = (secrets.rustfs or { }).secretKey or "";
  };
}
