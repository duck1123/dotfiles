{ lib, ... }:
with lib;
let
  typesLib = import ./types.nix { inherit lib; };
  inherit (typesLib) identitySubmodule;
in {
  options.identity = mkOption {
    type = types.nullOr identitySubmodule;
    description = "Identity information for the current user";
    default = null;
  };
}
