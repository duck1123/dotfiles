{ secrets, ... }:
{
  services.mariadb = {
    auth = {
      inherit (secrets.mariadb)
        database
        password
        replicationPassword
        rootPassword
        username
        ;
    };

    enable = true;
    hostAffinity = "edgenix";
    storageClassName = "longhorn";

    extraDatabases = [
      {
        name = "booklore";
        username = "booklore";
        password = secrets.booklore.database.password;
      }
      {
        name = "romm";
        username = "mariadb";
        password = secrets.mariadb.password;
      }
    ];
  };
}
