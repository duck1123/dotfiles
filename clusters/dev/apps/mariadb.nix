{ secrets, ... }:
{
  services.mariadb = {
    allowControlPlane = true;
    auth = {
      inherit (secrets.mariadb)
        database
        password
        rootPassword
        username
        ;
    };

    enable = true;
    hostAffinity = "edgenix";
    storageClassName = "longhorn";

    # Nightly backups go to the NAS's DatabaseBackups share, in mariadb/.
    nfs.enable = true;
    nfsSubPath = "DatabaseBackups";

    # Databases come from the apps that use them (uses-database +
    # databaseTarget = "mariadb"); see config.databases.

    # `data` is now the fresh volume from the bitnami->groundhog2k chart
    # migration (dump/restore cutover, see IMAGE-VERSIONS.md) -- confirmed
    # stable, so re-pinned here to survive a future disable/re-enable cycle.
    # The old bitnami-formatted volume (pvc-7ef8145c-...) is left orphaned
    # (Retain policy) as a rollback safety net.
    #
    # Captured via `kubectl get pv <name> -o jsonpath='{.spec.csi.volumeHandle}'`
    # -- see docs/pinned-volumes.md. Specific to this cluster.
    volumeOverrides = {
      data.volumeHandle = "pvc-42f3e481-1462-4e9c-95ae-7f7025ff5d8b";
      backups.volumeHandle = "pvc-f42c562c-5275-4ae2-999c-94eab513bcd9";
    };
  };
}
