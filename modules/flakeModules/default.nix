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
  # config = {
  #   hosts =
  #     if config ? _module && config._module ? args && config._module.args ? hosts then
  #       let h = config._module.args.hosts;
  #       in builtins.trace "DEBUG: hosts = ${builtins.toJSON h}" (
  #         if h ? pixel8 then h
  #         else throw "The host 'pixel8' must be present in the hosts attrset!"
  #       )
  #     else {};
  # };
}
