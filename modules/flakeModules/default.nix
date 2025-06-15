{ config, lib, ... }:
with lib;
let
  hostSubmodule = types.submodule {
    options = {
      id = mkOption {
        type = types.str;
        description = "The id";
      };
    };
  };
in {
  options = {
    hosts = mkOption {
      type = types.attrsOf hostSubmodule;
      description = "Map of hosts with their configurations";
      default = { };
    };
  };
}
