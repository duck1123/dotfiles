{ lib, ... }:
{
  flake.modules.generic.ageRecipients =
    { ... }:
    {
      options.ageRecipients = lib.mkOption {
        type = lib.types.str;
        default = { };
        description = "The age key that should be used to encrypt sops secrets";
      };
    };
}
