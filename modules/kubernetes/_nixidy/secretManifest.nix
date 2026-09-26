# Aggregates sopsSecretsManifest from all services for CI (one command: encrypt then build).
{ config, lib, ... }:
{
  options.nixidy.secretManifest = lib.mkOption {
    type = lib.types.listOf lib.types.attrs;
    default = [ ];
    description = "Aggregated list of { app, secretName, namespace, keys } for CI to encrypt.";
  };

  config.nixidy.secretManifest = lib.concatLists (
    lib.mapAttrsToList (_n: c: c.sopsSecretsManifest or [ ]) (config.services or { })
  );
}
