{ ... }:
{
  flake.types.generic.home-manager-submodule =
    { lib, ... }:
    with lib;
    types.submodule {
      options = {
        enable = mkOption {
          type = types.bool;
          default = false;
          description = "Enable home-manager configuration";
        };
      };
    };
}
