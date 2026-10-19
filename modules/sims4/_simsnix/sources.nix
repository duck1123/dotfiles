# Turns a list of catalog sources into modules, so that later sources win.
#
# A source is a directory (a path, read in place; nothing is copied to the
# store) with any of:
#   default.nix                  a module (config, profiles, overrides)
#   mods/<creator>/<mod>.json    tool-written data for sims4.mods.<creator>.<mod>
#   mods/<creator>/<mod>.nix     a hand-written definition of the same mod: the
#                                body of the mod's submodule, so it can declare
#                                options of its own
#
# Merging (D12): attrsets merge (mods, versions, profiles); everything else, a
# scalar or a whole list, is resolved by source order, later wins. That's done
# with priorities: the last source is left as is (priority 100, like any plain
# definition), the one before it gets 101, and so on. A definition that
# already has a priority (mkDefault, mkForce, ...) keeps it, so a mkDefault in
# any source is weaker than every other source's plain definitions.
{ lib }:
let
  # Keys the module system reads from a module instead of treating as config
  moduleKeys = [
    "_class"
    "_file"
    "disabledModules"
    "freeformType"
    "imports"
    "key"
    "meta"
  ];

  isModuleLike = v: lib.isAttrs v && (v ? options || v ? config || v ? imports);

  # Wrap every definition in a module with priority `prio`
  wrapModule =
    prio: m:
    if lib.isPath m || (lib.isString m && lib.hasPrefix "/" m) then
      {
        _file = toString m;
        key = toString m;
        imports = [ (wrapModule prio (import m)) ];
      }
    else if lib.isFunction m then
      lib.setFunctionArgs (args: wrapModule prio (m args)) (lib.functionArgs m)
    else
      let
        full = m ? config || m ? options;
        cfg = if full then m.config or { } else removeAttrs m moduleKeys;
      in
      removeAttrs m (if full then [ "config" ] else lib.attrNames cfg)
      // {
        imports = map (wrapModule prio) (m.imports or [ ]);
        config = wrapConfig prio cfg;
      };

  wrapConfig =
    prio: v:
    if lib.isFunction v then
      # a submodule definition (no option here takes a function or a path)
      wrapModule prio v
    else if lib.isPath v || isModuleLike v then
      # types.submodule reads an attrset definition as config only, so hand it
      # the wrapped module as a function
      _: wrapModule prio v
    else if lib.isAttrs v && v ? _type then
      {
        override = v;
        option = v;
        "if" = v // {
          content = wrapConfig prio v.content;
        };
        order = v // {
          content = wrapConfig prio v.content;
        };
        merge = v // {
          contents = map (wrapConfig prio) v.contents;
        };
      }
      .${v._type} or (lib.mkOverride prio v)
    else if lib.isAttrs v && !lib.isDerivation v then
      lib.mapAttrs (_: wrapConfig prio) v
    else
      lib.mkOverride prio v;

  readDirOf = dir: if builtins.pathExists dir then builtins.readDir dir else { };

  # The mods/ folder of a source as one module
  modsModule =
    src:
    let
      dir = src + "/mods";
      creators = lib.attrNames (lib.filterAttrs (_: t: t == "directory") (readDirOf dir));
      defsOf =
        creator:
        lib.mapAttrsToList
          (
            file: _:
            let
              path = dir + "/${creator}/${file}";
              mod = lib.removeSuffix ".json" (lib.removeSuffix ".nix" file);
            in
            {
              sims4.mods.${creator}.${mod} = if lib.hasSuffix ".json" file then lib.importJSON path else path;
            }
          )
          (
            lib.filterAttrs (f: t: t == "regular" && (lib.hasSuffix ".json" f || lib.hasSuffix ".nix" f)) (
              readDirOf (dir + "/${creator}")
            )
          );
    in
    {
      _file = toString dir;
      imports = lib.concatMap defsOf creators;
    };

  sourceModules =
    src:
    [ (modsModule src) ]
    ++ lib.optional (builtins.pathExists (src + "/default.nix")) (src + "/default.nix");
in
{
  inherit wrapModule;

  # [ src ... ] (first = weakest) -> [ module ... ]
  modulesFor =
    sources:
    let
      n = lib.length sources;
    in
    lib.concatLists (
      lib.imap0 (
        i: src:
        let
          mods = sourceModules src;
        in
        if i == n - 1 then mods else map (wrapModule (100 + n - 1 - i)) mods
      ) sources
    );
}
