{ config, lib, ... }:
let
  inherit (lib) mkOption types;
in
{
  options.nestedModules = mkOption {
    type = types.attrsOf (types.attrsOf (types.lazyAttrsOf types.raw));
    default = { };
    description = ''
      Modules published as `modules.<class>.<group>.<name>`. flake-parts types
      flake.modules.<class>.<name> as a deferredModule, so a nested group can't
      be declared there; registries (features, environments) put their groups
      here and they are written into the published output instead. They are
      visible as `inputs.self.modules.<class>.<group>.<name>` but not in
      `config.flake.modules`.
    '';
  };

  # `finish` takes a single definition, so every group goes through this one.
  config.touchup.attr.modules.finish =
    modules:
    modules // lib.mapAttrs (class: groups: (modules.${class} or { }) // groups) config.nestedModules;
}
