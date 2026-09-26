# Loads top-level registry directories: every `<dir>/<name>.nix` becomes a
# definition of `<attr>.<name>` in flake-parts, so adding an entry is just
# dropping a file in (no list to edit). A file holds only the entry's body, as
# a value or as a function of flake-parts module args (e.g. `{ config, ... }:`
# to reference other entries). Files and directories starting with `_` are
# skipped, like import-tree does.
#
# A "raw" registry keeps each file's value as-is, even when it is a function,
# for registries whose entries are themselves lambdas (e.g. appTemplates,
# whose bodies are nixidy module args -> mkArgoApp spec).
{ lib, ... }:
let
  loadRegistry =
    {
      raw ? false,
    }:
    attr: dir:
    let
      isEntry = n: t: t == "regular" && lib.hasSuffix ".nix" n && !lib.hasPrefix "_" n;

      toModule =
        file:
        let
          path = dir + "/${file}";
          body = import path;
          bodyFn = if lib.isFunction body && !raw then body else _: body;
        in
        {
          _file = path;
          # Advertise the body's own args so the module system supplies them
          # (same trick as the feature gate in ./features.nix).
          imports = [
            (lib.setFunctionArgs (args: {
              ${attr}.${lib.removeSuffix ".nix" file} = bodyFn args;
            }) (lib.functionArgs bodyFn))
          ];
        };
    in
    {
      imports = map toModule (builtins.attrNames (lib.filterAttrs isEntry (builtins.readDir dir)));
    };
in
{
  imports =
    lib.mapAttrsToList (loadRegistry { }) {
      environments = ../../environments;
      features = ../../features;
      hosts = ../../hosts;
      identities = ../../identities;
    }
    ++ lib.mapAttrsToList (loadRegistry { raw = true; }) {
      appTemplates = ../../appTemplates;
    };
}
