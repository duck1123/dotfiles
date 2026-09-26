# Registry for `appTemplates.<name>`, loaded (raw) from ../../appTemplates by
# ./registries.nix. Each entry is the lambda passed to `mkArgoApp`: nixidy
# module args (`{ config, lib, pkgs, self, ... }`) -> mkArgoApp spec. They are
# turned into `flake.nixidyApps` by modules/kubernetes/applications.nix.
{ lib, ... }:
{
  options.appTemplates = lib.mkOption {
    type = lib.types.attrsOf lib.types.raw;
    default = { };
    description = "nixidy application templates (module args -> mkArgoApp spec), keyed by app name.";
  };
}
