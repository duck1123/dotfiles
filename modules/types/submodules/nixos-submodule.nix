_: {
  flake.types.generic.nixos-submodule =
    { lib, ... }:
    with lib;
    types.submodule {
      options = {
        enable = mkOption {
          type = types.bool;
          default = false;
          description = "Enable nixos configuration";
        };
      };
    };
}
