{ ... }:
{
  flake.types.generic.identitySubmodule =
    { lib, ... }:
    with lib;
    types.submodule {
      options = {
        name = mkOption {
          type = types.str;
          description = "Full name of the user";
        };
        username = mkOption {
          type = types.str;
          description = "Username";
        };
        email = mkOption {
          type = types.str;
          description = "Email address";
        };
        gpgKey = mkOption {
          type = types.str;
          description = "GPG key ID";
        };
      };
    };
}
