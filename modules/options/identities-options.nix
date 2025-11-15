{ ... }: {
  flake.modules.generic.identities-options = { inputs, lib, ... }:
    let
      inherit (inputs.self.types) generic;
      identitySubmodule = generic.identitySubmodule { inherit inputs lib; };
    in with lib; {
      options.identities = mkOption {
        type = types.attrsOf identitySubmodule;
        description = "Map of identities with their configurations";
        default = { };
      };
    };
}

