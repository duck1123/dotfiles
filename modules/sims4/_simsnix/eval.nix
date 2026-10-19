# Entry point: evaluate catalog sources into a checked manifest.
#
#   nix eval --impure --json --expr '
#     import <framework>/eval.nix {
#       lib = import <nixpkgs>/lib;
#       sources = [ /path/to/public-source /path/to/private/sims4 ];  # later wins
#       profile = "default";
#     }'
#
# Sources are plain paths, read in place, so a catalog outside the repo never
# enters /nix/store. Returns the manifest, or throws listing every failed
# check. For tests: `extraModules` are added unwrapped after the sources, and
# `raw = true` returns { config, failures, manifest } instead of throwing.
{
  lib,
  sources,
  profile ? null,
  extraModules ? [ ],
  raw ? false,
}:
let
  inherit (import ./sources.nix { inherit lib; }) modulesFor;

  eval = lib.evalModules {
    modules = [
      ./options.nix
      ./profiles.nix
      ./manifest.nix
      { sims4.profile = profile; }
    ]
    ++ modulesFor sources
    ++ extraModules;
  };

  cfg = eval.config.sims4;
  failed = map (a: a.message) (lib.filter (a: !a.assertion) cfg.assertions);
in
if raw then
  {
    config = cfg;
    failures = failed;
    inherit (cfg) manifest;
  }
else if failed == [ ] then
  cfg.manifest
else
  throw "simsnix: ${toString (lib.length failed)} problem(s):\n${
    lib.concatMapStringsSep "\n" (m: "- ${m}") failed
  }"
