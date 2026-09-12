# The Two Module Systems

This repo actually evaluates **two separate NixOS-style module systems**, and it's easy to assume there's only one because both feel the same (`options`/`config`, `mkOption`, lazy cross-references). Mixing them up is a real time sink — a new shared option can look correctly wired and still fail with "option does not exist".

## Outer: flake-parts (`./modules`, fully auto-imported)

`flake.nix` does:

```nix
outputs = inputs: inputs.flake-parts.lib.mkFlake { inherit inputs; } (inputs.import-tree ./modules);
```

Every `.nix` file under `modules/` (recursively) is imported automatically via `import-tree` — this is the "drop a file in, no registration needed" behavior described for `modules/pkgs/<name>-site.nix` in the main CLAUDE.md. This is flake-parts' own top-level module system: `perSystem`, `flake.*`, and any custom top-level options a module chooses to declare (e.g. `options.flake.lib`, `options.flake.nixidyApps` in `modules/flake/flake-parts.nix`).

## Inner: nixidy's per-environment eval (`modules/nixidyEnvs.nix`, explicit list only)

Separately, `modules/nixidyEnvs.nix` calls `inputs.nixidy.lib.mkEnvs`, which runs its **own, independent** `evalModules` per environment (currently just `dev`). This is where `services.<name>`, `config.ingressProviders`, `config.nfsTargets`, `config.databaseProviders`, `config.homepageGroups`, `config.devDefaults`, and everything else that `applications/*.nix` and `env/dev/*.nix` reference actually lives. Its module list is **hand-maintained, not auto-imported**:

```nix
devEnv = inputs.nixidy.lib.mkEnvs {
  envs.dev.modules = [ ../env/dev.nix ];
  modules = (builtins.attrValues self.nixidyApps) ++ [
    self.modules.generic.ageRecipients
    ./secretManifest.nix
    ./secretSpecs.nix
    ./nodeProfiles.nix
    ./ingressProviders.nix
    ./nfsTargets.nix
    ./databaseProviders.nix
    ./homepageGroups.nix
  ];
};
```

`(builtins.attrValues self.nixidyApps)` pulls in every `applications/*.nix` file automatically (those register themselves via `flake.nixidyApps.<name>` in the outer system, then get fed into the inner one here) — **that** part is effectively auto-wired. But a shared, repo-wide **registry-style option** (an `options.foo = mkOption {...}` meant to be referenced from multiple `applications/*.nix`/`env/dev/*.nix` files the way `ingressProviders`/`nfsTargets`/`databaseProviders`/`homepageGroups` are) has to be added to this list by hand.

## The failure mode

If you add a new file like `modules/nfsTargets.nix`'s siblings (same shape: `{ lib, ... }: { options.someRegistry = lib.mkOption { ... }; }`) but forget to add it to the list above, you get exactly this — confusing because the file is 100% correctly written and *would* work if only it were in the list:

```
error: The option `someRegistry' does not exist. Definition values:
- In `.../env/dev.nix':
    [ ... ]
Did you mean `templates', `devDefaults' or `nfsTargets'?
```

The "Did you mean" suggestions are a tell: they're all names that *are* in the explicit list, confirming the module system found your definition site but has no matching option declaration because your file was never imported into *this* eval.

## How to apply

When adding a new shared/cross-app config registry (anything meant to be referenced as `config.<name>` from more than one `applications/*.nix` or `env/dev/*.nix` file):

1. Create it as its own file under `modules/` (top level, alongside `ingressProviders.nix`/`nfsTargets.nix`/`databaseProviders.nix`/`homepageGroups.nix` — not `modules/lib/`, that's for `flake.lib.*` helper functions, a different thing entirely).
2. `git add` it (see the git-tracking gotcha in the main CLAUDE.md — untracked files are invisible to the flake regardless of which module system).
3. Add it to the `modules` list in `modules/nixidyEnvs.nix`.
4. Only then will `env/dev.nix` (or any `env/dev/*.nix`) be able to set it, and `modules/lib/mkArgoApp.nix` (or any `applications/*.nix`) be able to read it via `config.<name>`.

Options declared *inside* an individual `applications/<name>.nix` (via that app's own `extraOptions`) don't need any of this — those are per-app `services.<name>.*` options, always available once the app module itself is registered via `flake.nixidyApps.<name>` (which happens automatically). This gotcha is specifically about options meant to be shared *across* apps.
