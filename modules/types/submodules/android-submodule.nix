{ ... }: {
  flake.types.generic.android-submodule = { lib, ... }:
    with lib;
    types.submodule {
      options = {
        enable = mkOption {
          type = types.bool;
          default = false;
          description = "Enable android configuration";
        };
      };
    };
}
