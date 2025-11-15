{ ... }: {
  flake.types.generic.media-submodule = { lib, ... }:
    with lib;
    types.submodule {
      options = {
        enable = mkOption {
          type = types.bool;
          default = false;
          description = "Enable media feature";
        };

        server = mkOption {
          type = types.submodule {
            options = {
              enable = mkOption {
                type = types.bool;
                default = false;
                description = "Enable media server";
              };
            };
          };
          default = { };
          description = "Media server configuration";
        };
      };
    };
}
