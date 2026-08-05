{ secrets, ... }:
{
  services.redis = {
    enable = true;
    hostAffinity = "edgenix";
    password = secrets.redis.password;
  };
}
