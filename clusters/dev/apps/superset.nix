{ secrets, ... }:
{
  services.superset = {
    admin = {
      username = (secrets.superset or { }).admin.username or "admin";
      email = (secrets.superset or { }).admin.email or "admin@superset.local";
      password = (secrets.superset or { }).admin.password or "";
    };

    database.password = (secrets.superset or { }).database.password or "";
    databaseTarget = "postgresql";

    homepage.group = "Analytics";
    ingress.tls.enable = true;
    ingressProvider = "traefik-lan";
    redisTarget = "redis";
    secretKey = (secrets.superset or { }).secretKey or "";
  };
}
