{ config, lib, ... }:
with lib;
let
  hostSubmodule = types.submodule {
    options = {
      id = mkOption {
        type = types.int;
        description = "The id";
      };
    };
  };
in {
  options = {
    host = mkOption {
      type = hostSubmodule;
      description = "The host";
      default = { };
    };

    hosts = mkOption {
      type = types.attrsOf hostSubmodule;
      description = "Map of hosts with their configurations";
      default = { };
    };
  };
}
