{ ... }: {
  flake.modules.options.hosts = { inputs, lib, ... }:
    with lib;
    let
      typesLib = inputs.self.modules.options.types { inherit lib; };
      inherit (typesLib) hostSubmodule;
    in {
      options.hosts = mkOption {
        type = types.attrsOf hostSubmodule;
        description = "Map of hosts with their configurations";
        default = { };
      };
    };
}
