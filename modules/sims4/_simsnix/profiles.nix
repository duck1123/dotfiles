# Resolves sims4.profile into sims4.activeMods: the base profile's mods (if
# any), minus `remove`, plus `mods`. Unknown profiles and mod ids are reported
# as assertions rather than thrown, so every problem shows at once.
{ config, lib, ... }:
let
  cfg = config.sims4;

  allIds = lib.concatLists (
    lib.mapAttrsToList (creator: mods: map (m: "${creator}/${m}") (lib.attrNames mods)) cfg.mods
  );

  # `seen` stops a base cycle (a -> b -> a) from recursing forever
  resolve =
    seen: name:
    let
      p = cfg.profiles.${name};
      base =
        if p.base == null || lib.elem p.base seen || !(cfg.profiles ? ${p.base}) then
          [ ]
        else
          resolve (seen ++ [ name ]) p.base;
    in
    lib.unique ((lib.subtractLists p.remove base) ++ p.mods);

  baseProblems = lib.concatLists (
    lib.mapAttrsToList (
      name: p:
      lib.optional (
        p.base != null && !(cfg.profiles ? ${p.base})
      ) "profile ${name}: unknown base profile ${p.base}"
      ++ lib.optional (p.base == name) "profile ${name}: is its own base"
    ) cfg.profiles
  );

  unknownIds = lib.concatLists (
    lib.mapAttrsToList (
      name: p:
      map (id: "profile ${name}: unknown mod ${id}") (
        lib.filter (id: !(lib.elem id allIds)) (p.mods ++ p.remove)
      )
    ) cfg.profiles
  );
in
{
  config.sims4 = {
    activeMods =
      if cfg.profile != null && cfg.profiles ? ${cfg.profile} then resolve [ ] cfg.profile else [ ];

    assertions = [
      {
        assertion = cfg.profile == null || cfg.profiles ? ${cfg.profile};
        message = "unknown profile ${toString cfg.profile} (known: ${lib.concatStringsSep ", " (lib.attrNames cfg.profiles)})";
      }
    ]
    ++ map (message: {
      assertion = false;
      inherit message;
    }) (baseProblems ++ unknownIds);
  };
}
