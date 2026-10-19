# Computes sims4.manifest from the enabled mods, and checks it.
#
# manifest = {
#   profile;
#   mods  = [ { id, version, folder, stateFiles } ];
#   files = [ { mod, version, target, sha256 } ];   # target is relative to Mods/
# }
#
# An archive's members are installed, not the archive. A file is installed if
# its (stripped) path matches one of the mod's `include` regexes and none of
# its `exclude` ones, compared lowercased. Targets, folders and depth limits
# are compared case-insensitively, as the game sees them under Wine.
{ config, lib, ... }:
let
  cfg = config.sims4;

  allMods = lib.concatLists (lib.mapAttrsToList (_: lib.attrValues) cfg.mods);
  enabled = lib.filter (m: m.enable) allMods;
  enabledIds = map (m: m.id) enabled;
  allIds = map (m: m.id) allMods;

  matchesAny = res: path: lib.any (re: builtins.match re (lib.toLower path) != null) res;

  strip =
    n: path: lib.concatStringsSep "/" (lib.drop n (lib.filter (c: c != "") (lib.splitString "/" path)));

  versionOf = m: if m.selectedVersion == null then null else m.versions.${m.selectedVersion} or null;

  filesOf =
    m:
    let
      v = versionOf m;
      entries = lib.concatMap (
        f:
        if f.members == [ ] then
          [
            {
              inherit (f) sha256;
              path = f.name;
            }
          ]
        else
          map (mem: {
            inherit (mem) sha256;
            path = strip m.stripComponents mem.path;
          }) f.members
      ) (if v == null then [ ] else v.files);
      wanted = lib.filter (
        e: e.path != "" && matchesAny m.include e.path && !(matchesAny m.exclude e.path)
      ) entries;
    in
    map (e: {
      mod = m.id;
      version = m.selectedVersion;
      target = if m.folder == null then e.path else "${m.folder}/${e.path}";
      inherit (e) sha256;
    }) wanted;

  files = lib.sort (a: b: a.target < b.target) (lib.concatMap filesOf enabled);

  depth = target: lib.length (lib.splitString "/" target) - 1;
  hasExt = ext: target: lib.hasSuffix ext (lib.toLower target);

  # [ { key, items } ] for keys used by more than one item
  duplicates =
    keyOf: items:
    lib.filter (g: lib.length g.items > 1) (
      lib.mapAttrsToList (key: items: { inherit key items; }) (lib.groupBy keyOf items)
    );

  fail = message: {
    assertion = false;
    inherit message;
  };

  modProblems = lib.concatMap (
    m:
    lib.optional (m.selectedVersion == null) (fail "${m.id}: enabled but has no versions")
    ++ lib.optional (m.selectedVersion != null && versionOf m == null) (
      fail "${m.id}: unknown version ${m.selectedVersion} (known: ${lib.concatStringsSep ", " (lib.attrNames m.versions)})"
    )
    ++ lib.optional (versionOf m != null && filesOf m == [ ]) (
      fail "${m.id}: version ${m.selectedVersion} installs no files (check include/exclude/stripComponents)"
    )
    ++ map (
      r: fail "${m.id}: requires ${r}, which is ${if lib.elem r allIds then "not enabled" else "unknown"}"
    ) (lib.filter (r: !(lib.elem r enabledIds)) m.requires)
    ++ map (c: fail "${m.id}: conflicts with ${c}, which is enabled") (
      lib.filter (c: lib.elem c enabledIds) m.conflicts
    )
  ) enabled;

  fileProblems =
    map (f: fail "${f.mod}: ${f.target}: .ts4script files load at most one folder below Mods/") (
      lib.filter (f: hasExt ".ts4script" f.target && depth f.target > 1) files
    )
    ++ map (f: fail "${f.mod}: ${f.target}: .package files load at most five folders below Mods/") (
      lib.filter (f: hasExt ".package" f.target && depth f.target > 5) files
    )
    ++ map (
      g:
      fail "${g.key}: installed by more than one file (${
        lib.concatMapStringsSep ", " (f: "${f.mod}@${f.version}") g.items
      })"
    ) (duplicates (f: lib.toLower f.target) files)
    ++ map (g: fail "folder ${g.key} is used by ${lib.concatMapStringsSep ", " (m: m.id) g.items}") (
      duplicates (m: lib.toLower m.folder) (lib.filter (m: m.folder != null) enabled)
    );
in
{
  config.sims4 = {
    assertions = modProblems ++ fileProblems;

    manifest = {
      inherit (cfg) profile;
      mods = map (m: {
        inherit (m) id folder stateFiles;
        version = m.selectedVersion;
      }) enabled;
      inherit files;
    };
  };
}
