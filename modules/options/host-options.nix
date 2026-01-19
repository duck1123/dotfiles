{ ... }:
{
  flake.modules.generic.host-options =
    { inputs, lib, ... }:
    let
      inherit (inputs.self.types) generic;
      hostSubmodule = generic.hostSubmodule { inherit inputs lib; };
    in
    with lib;
    {
      options.host = mkOption {
        type = hostSubmodule;
        description = "Info about a host";
      };
    };
}
