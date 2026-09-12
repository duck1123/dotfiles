{ config, secrets, ... }:
{
  services.garage = {
    enable = true;

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

    # Start out on longhorn (no NFS) to keep the first boot simple — flip on
    # once garage is validated, pointed at its own NAS export like rustfs's.
    nfs.enable = false;

    # Captured via `kubectl get pv <name> -o jsonpath='{.spec.csi.volumeHandle}'`
    # -- see docs/pinned-volumes.md. Specific to this cluster. dataVolumeHandle
    # only takes effect while nfs.enable is false, as above.
    volumeOverrides.meta.volumeHandle = "pvc-12507954-e2c5-4fd6-9a00-498f96993445";
    dataVolumeHandle = "pvc-06689573-2c6c-4acd-961d-95a4801239b2";
  };
}
