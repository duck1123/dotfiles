{ ... }: {
  flake.modules.generic.host-options = { inputs, lib, ... }:
    with lib;
    let
      typesLib = inputs.self.modules.options.types { inherit lib; };
      inherit (typesLib) hostSubmodule;
    in {
      options.host = mkOption {
        type = hostSubmodule;
        description = "Info about a host";
      };
    };
}
