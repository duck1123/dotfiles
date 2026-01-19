{ ... }:
{
  flake.types.generic.feature-submodule =
    { inputs, lib, ... }:
    with lib;
    types.submodule {
      options = mapAttrs (name: optionFn: optionFn { inherit inputs lib; }) (
        inputs.self.types.generic.feature-options or { }
      );
    };
}
