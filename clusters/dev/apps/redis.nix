{ secrets, ... }:
{
  services.redis = {
    hostAffinity = "edgenix";
    password = secrets.redis.password;
    repairAof = false;
  };
}
