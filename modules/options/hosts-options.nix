{ ... }:
{
  flake.modules.generic.hosts-options =
    { inputs, lib, ... }:
    let
      inherit (inputs.self.types) generic;
      hostSubmodule = generic.hostSubmodule { inherit inputs lib; };
    in
    with lib;
    {
      options.hosts = mkOption {
        type = types.attrsOf hostSubmodule;
        description = "Map of hosts with their configurations";
        default = { };
      };
    };
}
