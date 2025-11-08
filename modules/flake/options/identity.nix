{ ... }: {
  flake.modules.options.identity = { inputs, lib, ... }:
    with lib;
    let
      typesLib = inputs.self.modules.options.types { inherit lib; };
      inherit (typesLib) identitySubmodule;
    in {
      options.identity = mkOption {
        type = types.nullOr identitySubmodule;
        description = "Identity information for the current user";
        default = null;
      };
    };
}
