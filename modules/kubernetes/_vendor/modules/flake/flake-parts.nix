# Import flake-parts modules module
{ inputs, lib, ... }:
{
  imports = [
    inputs.flake-parts.flakeModules.modules
  ];

  options.flake.lib = lib.mkOption {
    type = lib.types.lazyAttrsOf lib.types.unspecified;
    default = { };
    description = "Merged library functions for this flake.";
  };

  options.flake.nixidyApps = lib.mkOption {
    type = lib.types.attrsOf lib.types.raw;
    default = { };
    description = "nixidy application modules indexed by app name; collected into nixidy mkEnvs.modules.";
  };
}
