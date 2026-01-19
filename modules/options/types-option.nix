{ lib, ... }:
{
  options.flake.types = lib.mkOption {
    type = lib.types.submodule {
      options = {
        generic = lib.mkOption {
          type = lib.types.attrsOf lib.types.anything;
          description = "Generic type functions";
          default = { };
        };
      };
    };
    description = "Type functions for use in modules";
    default = { };
  };
}
