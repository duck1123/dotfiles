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
  };
}
