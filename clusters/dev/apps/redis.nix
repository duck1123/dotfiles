{ secrets, ... }:
{
  services.redis = {
    enable = true;
    hostAffinity = "edgenix";
    password = secrets.redis.password;
    replicas = 1;
    repairAof = false;
  };
}
