_:
let
  simpleFeatureWith =
    { lib, ... }:
    description: extraOptions:
    with lib;
    mkOption {
      type = types.submodule {
        options = {
          enable = mkOption {
            type = types.bool;
            default = false;
            description = "Enable ${description}";
          };
        }
        // extraOptions;
      };
      default = { };
      description = "${description} configuration";
    };
in
{
  flake.types.generic = {
    # `enable` plus any extra per-feature options
    inherit simpleFeatureWith;

    simpleFeature = args: description: simpleFeatureWith args description { };
  };
}
