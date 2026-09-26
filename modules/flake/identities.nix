# Registry for `identities.<name>`, loaded from ../../identities by
# ./registries.nix. Evaluated once at the flake level and published as
# `inputs.self.identities`; generic modules see it as `config.identities`
# (modules/options/identities-options.nix).
{
  config,
  inputs,
  lib,
  ...
}:
{
  options.identities = lib.mkOption {
    type = lib.types.attrsOf (config.flake.types.generic.identitySubmodule { inherit inputs lib; });
    default = { };
    description = "Map of identities with their configurations";
  };

  config.flake.identities = config.identities;
}
