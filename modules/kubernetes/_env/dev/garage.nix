{ config, secrets, ... }:
{
  services.garage = {
    enable = true;

    # Metadata and data live in a plain directory on nixmini's NVMe. They
    # used to be single-replica Longhorn volumes on nasnix, the NAS VM, whose
    # one virtual disk saturates whenever the NAS is busy; the metadata DB was
    # corrupted three times in two weeks (LMDB 2026-09-13 and 2026-09-20,
    # sqlite 2026-09-26). See docs/nix-csi-and-binary-cache.md.
    hostAffinity = "nixmini";
    hostPath = "/var/lib/garage";

    adminToken = (secrets.garage or { }).adminToken or "";
    rpcSecret = (secrets.garage or { }).rpcSecret or "";
    accessKey = (secrets.garage or { }).accessKey or "";
    secretKey = (secrets.garage or { }).secretKey or "";

    homepage.group = "Storage";

    ingressProvider = "traefik-lan";
    ingress.tls.enable = true;

    # The ingress only routes to the S3 API (port 3900), which correctly
    # 403s anonymous requests -- not a usable health check. The admin API
    # (port 3903) serves an unauthenticated /health, same as the pod's own
    # liveness/readiness probes, but isn't exposed through the ingress -- so
    # hit it over cluster-internal service DNS instead of adding a public
    # route just for this.
    monitoring.autokuma = {
      enable = true;
      url = "http://garage.garage.svc.cluster.local:3903/health";
    };
  };
}
