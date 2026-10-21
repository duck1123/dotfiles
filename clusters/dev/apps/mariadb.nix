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

    # The `data` pin (volume-pins.json) is the fresh volume from the
    # bitnami->groundhog2k chart migration (dump/restore cutover, see
    # IMAGE-VERSIONS.md), confirmed stable. The old bitnami-formatted volume
    # (pvc-7ef8145c-...) is left orphaned (Retain policy) as a rollback
    # safety net.
  };
}
