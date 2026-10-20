{ secrets, ... }:
{
  services.postgresql = {
    auth = {
      inherit (secrets.postgresql)
        adminPassword
        adminUsername
        replicationPassword
        userPassword
        ;
    };

    enable = true;
    hostAffinity = "edgenix";
    storageClassName = "longhorn";

    # Nightly backups go to the NAS's DatabaseBackups share, in postgresql/.
    nfs.enable = true;
    nfsSubPath = "DatabaseBackups";

    # Databases come from the apps that use them (uses-database +
    # databaseTarget = "postgresql"); see config.databases.

    # Captured via `kubectl get pv <name> -o jsonpath='{.spec.csi.volumeHandle}'`
    # -- see docs/pinned-volumes.md. Specific to this cluster.
    volumeOverrides = {
      data.volumeHandle = "pvc-f011e680-b80d-4628-abbd-8641d837938b";
      backups.volumeHandle = "pvc-4a9e7907-8262-4938-b52b-3b5971935619";
    };
  };
}
