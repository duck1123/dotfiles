# A hand-written definition with an option of its own
{ config, lib, ... }:
{
  options.variant = lib.mkOption {
    type = lib.types.enum [
      "full"
      "lite"
    ];
    default = "full";
  };
  config = {
    requires = [ "acme/core" ];
    exclude = lib.optional (config.variant == "lite") ".*extras\\.package";
    versions."1" = {
      files = [
        {
          name = "variants.package";
          sha256 = "d100";
        }
        {
          name = "variants-extras.package";
          sha256 = "d101";
        }
      ];
    };
  };
}
