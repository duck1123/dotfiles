{ config, secrets, ... }:
{
  services.nocodb =
    let
      storage-backend = "rustfs";
    in
    {
      allowLocalExternalDatabases = true;
      auth.jwtSecret = (secrets.nocodb or { }).jwtSecret or "";
      enable = true;

      ingress = {
        domain = "nocodb.${config.devDefaults.tailDomain}";
        ingressClassName = "tailscale";
        clusterIssuer = "tailscale";
      };

      database = {
        host = "postgresql.postgresql";
        port = 5432;
        name = "nocodb";
        username = "nocodb";
        password = (secrets.nocodb.postgresql or { }).password or secrets.postgresql.userPassword;
      };

      redis = {
        host = "redis.redis";
        port = 6379;
        password = secrets.redis.password;
      };

      storage =
        if storage-backend == "rustfs" then
          {
            enable = true;
            backend = "rustfs";
            bucketName = (secrets.rustfs or { }).bucketName or "nocodb";
            endpoint = "http://rustfs.rustfs:9000";
            region = (secrets.rustfs or { }).region or "us-east-1";
            accessKey = (secrets.rustfs or { }).accessKey or "";
            secretKey = (secrets.rustfs or { }).secretKey or "";
          }
        else
          {
            enable = true;
            backend = "minio";
            bucketName = (secrets.nocodb.minio or { }).bucketName or "nocodb";
            endpoint = "http://minio.minio:9000";
            region = (secrets.nocodb.minio or { }).region or "us-east-1";
            accessKey = (secrets.nocodb.minio or { }).rootUser or "";
            secretKey = (secrets.nocodb.minio or { }).rootPassword or "";
          };

      publicUrl = "https://nocodb.${config.devDefaults.tailDomain}";
    };
}
