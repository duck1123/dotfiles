{ lib, ... }:
{
  options.redisProviders = lib.mkOption {
    description = ''
      Named redis providers. Keyed by name (i.e. the value used in
      services.<name>.redisTarget). mkArgoApp uses these as mkDefault
      values for the redis host, port, and password of apps built with
      uses-redis.

      passwordFor is a function of the app name, the same as
      databaseProviders, so a provider can hand each app its own
      credentials. Database indices are not part of a provider: they're
      per-app (see each app's redis.*DbIndex options).
    '';
    type = lib.types.attrsOf (
      lib.types.submodule {
        options = {
          host = lib.mkOption {
            description = "Redis host (cluster DNS, or a node address).";
            type = lib.types.str;
          };
          port = lib.mkOption {
            description = "Redis port.";
            type = lib.types.port;
          };
          passwordFor = lib.mkOption {
            description = "Function from app name to the default redis password for that app.";
            type = lib.types.functionTo lib.types.str;
          };
        };
      }
    );
    default = { };
  };
}
