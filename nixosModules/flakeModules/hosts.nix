{ lib, ... }:
with lib;
let
  typesLib = import ./types.nix { inherit lib; };
  inherit (typesLib) simpleFeature identitySubmodule;

  hostSubmodule =
    typesLib.hostSubmodule { inherit simpleFeature identitySubmodule; };
in {
  options.hosts = mkOption {
    type = types.attrsOf hostSubmodule;
    description = "Map of hosts with their configurations";
    default = { };
  };
}
