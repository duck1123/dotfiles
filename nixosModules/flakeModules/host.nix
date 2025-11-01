{ lib, ... }:
with lib;
let
  typesLib = import ./types.nix { inherit lib; };
  inherit (typesLib) identitySubmodule simpleFeature;
  hostSubmodule =
    typesLib.hostSubmodule { inherit simpleFeature identitySubmodule; };
in {
  options.host = mkOption {
    type = hostSubmodule;
    description = "Info about a host";
  };
}
