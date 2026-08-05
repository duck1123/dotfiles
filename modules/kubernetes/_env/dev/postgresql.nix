{ secrets, arrDatabases, ... }:
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

    extraDatabases =
      arrDatabases [
        { name = "prowlarr"; }
        { name = "sonarr"; }
        { name = "radarr"; }
        { name = "lidarr"; }
        { name = "whisparr"; }
        { name = "listenarr"; }
      ]
      ++ [
        {
          name = "immich";
          username = "immich";
          password = secrets.postgresql.userPassword;
        }
        {
          name = "gitea";
          username = "postgres";
          password = secrets.postgresql.userPassword;
        }
        {
          name = "affine";
          username = "affine";
          password = secrets.postgresql.userPassword;
        }
        {
          name = "memos";
          username = "postgres";
          password = secrets.postgresql.userPassword;
        }
        {
          name = "nocodb";
          username = "nocodb";
          password = secrets.postgresql.userPassword;
        }
        {
          name = "romm";
          username = "postgres";
          password = secrets.postgresql.userPassword;
        }
      ];
  };
}
