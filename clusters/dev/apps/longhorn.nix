{ config, ... }:
{
  services.longhorn = {
    enable = true;
    # The NAS's ClusterBackups share (Longhorn owns backupstore/ in it; file
    # exports of volumes go next to it in exports/). NFSv3 like every other
    # NAS mount. soft, not hard: longhorn-manager mounts this itself, and a
    # hard mount would hang it whenever the NAS stalls; a failed write just
    # fails that backup.
    backupTarget = "nfs://${config.devDefaults.nasHost}:${config.devDefaults.nasBase}/ClusterBackups?nfsOptions=nfsvers=3,nolock,soft,timeo=600,retrans=3";

    homepage.group = "Storage";

    ingressProvider = "traefik-lan";
    ingress.tls.enable = true;
  };
}
