# Aggregates sopsSecretsSpec from all services for the external encryption script.
# Unlike secretManifest.nix (metadata only), this includes plaintext values so the
# shell script can encrypt them with sops outside of the Nix store.
# Access via: nix eval --impure --json .#nixidySecretSpecs.x86_64-linux.dev
{ config, lib, ... }:
{
  options.nixidy.secretSpecs = lib.mkOption {
    type = lib.types.listOf lib.types.attrs;
    default = [ ];
    description = "Aggregated list of { secretName, namespace, values } for external sops encryption.";
  };

  config.nixidy.secretSpecs = lib.concatLists (
    lib.mapAttrsToList (_n: c: c.sopsSecretsSpec or [ ]) (config.services or { })
  );
}
