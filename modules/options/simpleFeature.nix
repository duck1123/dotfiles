{ ... }:
{
  flake.types.generic.simpleFeature =
    { lib, ... }:
    description:
    with lib;
    mkOption {
      type = types.submodule {
        options = {
          enable = mkOption {
            type = types.bool;
            default = false;
            description = "Enable ${description}";
          };
        };
      };
      default = { };
      description = "${description} configuration";
    };
}
