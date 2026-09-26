# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Overview

This is a Nix flake-based dotfiles/system configuration repo managing multiple NixOS hosts and home-manager configurations. It uses [flake-parts](https://github.com/hercules-ci/flake-parts) + [import-tree](https://github.com/vic/import-tree) to auto-import all modules from `./modules/`.

The primary task runner is [nur](https://github.com/nur-taskrunner/nur) using Nushell, with tasks defined in `scripts/nur.nu`.

## Key Commands

```sh
# Build and apply local configuration
nur build                        # build current home + OS configs
nur switch                       # switch both home-manager and NixOS
nur switch home                  # apply home-manager config only
nur switch os                    # apply NixOS config only
nur switch k8s                   # build and push k8s manifests only (no --host)
nur switch os --boot             # set NixOS as boot default instead of activating (safe for slow activations)

# Validation and formatting
nur check           # run nix flake check
nur format          # format all .nix files with nixfmt
nur lint            # lint all .nix files with statix

# Remote deployment (builds locally, copies + activates remotely)
nur switch --host edgenix    # deploy to edgenix
nur switch --host nasnix     # deploy to nasnix
nur diff-os --host edgenix   # show package changes before deploying

# Flake maintenance
nix flake update    # update flake.lock
nur build --all     # build all configurations

# Cluster operations (see modules/kubernetes/docs/)
nur apps list                # list app names known to the cluster
nur apps restart <name>      # roll an app's Deployment/StatefulSet
nur argocd sync [name]       # trigger an ArgoCD sync (all apps, or just <name>)
nur argocd refresh [name]    # force ArgoCD to re-diff against git
nur postgres list            # list PostgreSQL databases + sizes
nur postgres backup          # dump all PostgreSQL databases to ./backups/postgresql
nur mariadb list-backups     # list MariaDB backups on the mariadb-backups PVC
nur kuma-cli config          # write ~/.config/kuma/config.toml from cluster secrets
nur forward argocd           # port-forward the ArgoCD UI to localhost:8080
```

## Architecture

### Module Organization (`modules/`)

The flake outputs are assembled from modules under `modules/`, auto-imported via `import-tree`. The entry point in `flake.nix` is:
```nix
outputs = inputs: inputs.flake-parts.lib.mkFlake { inherit inputs; } (inputs.import-tree ./modules);
```

Key subdirectories:
- `modules/flake/` — flake outputs: `nixosConfigurations`, `homeConfigurations`, `devShells`, `packages`, and the `lib/+mk-os.nix` helpers
- `modules/hosts/` — per-host module definitions (one `.nix` per host, e.g. `edgenix.nix`)
- `modules/features/` — features enabled/disabled per host (bluetooth, hyprland, kubernetes, etc.), each self-registering via `features.<name>` (see Feature System)
- `modules/environments/` — desktop environments (budgie, gnome, hyprland, i3, niri, plasma6), each self-registering via `environments.<name>` (see Environment System)
- `modules/nixos/` — NixOS-specific modules (boot, users, i18n, sddm, etc.)
- `modules/options/` — NixOS option declarations (host, hosts, identities, simpleFeature type)
- `modules/types/` — custom Nix types/submodules for hosts, identities, features
- `modules/identities/` — per-user identity definitions (duck, deck, drenfer)
- `modules/kubernetes/` — the k3s fleet-ops integration, fully consolidated into this repo (the `k3s-fleetops` flake input is gone; there is no external dependency left). `_vendor/applications/` and `_vendor/generators/`/`_vendor/lib/`/`_vendor/modules/` hold the application library/generators (edit these directly — "_vendor" is a historical name, not a sync boundary); `_env/dev/` has the per-app instance config for the `dev` nixidy environment. See `modules/kubernetes/docs/` for deployment workflow, the two-module-system gotcha, pinned-volume handling, and a troubleshooting playbook.

### Host Configuration Pattern

Each host file (e.g., `modules/hosts/edgenix.nix`) defines three namespaced modules in `flake.modules`:
1. `generic.<hostname>` — shared config: features enabled/disabled, identity assignment, syncthing shares
2. `homeManager.<hostname>` — home-manager extras: extra packages, session paths
3. `nixos.<hostname>` — NixOS hardware config (specialisations are generated from `hosts.<hostname>.environments`, see Environment System)

`modules/flake/nixosConfigurations.nix` builds hosts using helpers from `lib/+mk-os.nix`:
- `linux "hostname"` → `nixpkgs.lib.nixosSystem` with `modules.nixos.base` + `modules.nixos.<hostname>`
- `wsl "hostname"` → WSL variant

### Feature System

Features are toggled with `enable = true/false` under `hosts.<hostname>.features.<name>`.

Each feature is declared **once**, in `modules/features/<name>.nix`, by setting `features.<name>` in the registry defined in `modules/flake/features.nix`. That single declaration generates:
- the `hosts.<host>.features.<name>` option (via `simpleFeatureWith` in `modules/options/simpleFeature.nix`)
- `modules.homeManager.features.<name>` and `modules.nixos.features.<name>`, each wrapped in `mkIf hosts.<host>.features.<name>.enable`
- their inclusion in `homeManager.base` / `nixos.base` (which import every registered feature; there is no list to edit)

```nix
_: {
  features.vim = {
    # description = "...";            # optional; defaults to "<name> feature"
    homeManager = { pkgs, ... }: { home.packages = [ pkgs.neovim ]; };  # body only: no `config =` / `mkIf`
    nixos = _: { programs.vim.enable = true; };                         # optional, either class may be omitted
  };
}
```

Registry knobs beyond the two bodies (a body may also be a list of bodies):
- `extraOptions = { inputs, lib }: { foo = mkOption ...; };` — extra options next to `enable` (see `tailscale`, `nix`, `hyprland`)
- `option = { inputs, lib }: mkOption ...;` — replace the whole generated option with a custom type (see `media`, `kubernetes`)
- `gated = false` — skip the automatic `enable` gate when the feature has no plain `enable` or gates differently per class; the bodies then return their own `mkIf`/`mkMerge` (see `media`, `kubernetes`)

Gotchas:
- flake-parts types `flake.modules.<class>.<name>` as a `deferredModule`, so `features.<name>` can't be nested there. The registry sets `nestedModules.<class>.features` (`modules/flake/nested-modules.nix`), which is written into the *published* flake output through `touchup.attr.modules.finish`, so it is visible as `inputs.self.modules.<class>.features.<name>` but **not** in `config.flake.modules` inside flake-parts modules. `finish` only takes one definition, so any other nested group must go through `nestedModules` too.
- A body that sets options which only exist in some hosts (e.g. `sops.*`) can't live in a feature that WSL also imports, because `mkIf false` still errors on an undeclared option. That is why `modules/features/nix-attic.nix` stays a standalone `modules.nixos.nix-attic` imported directly by `nixos.base`.
- Non-feature modules (`state-version`, `boot`, `i18n`, `users`, `sddm`, `environments-*`) are still plain `flake.modules.<class>.<name>` and are listed explicitly in `base.nix`.

### Environment System

Desktop environments work like features. Each is declared once in `modules/environments/<name>.nix` by setting `environments.<name>` in the registry in `modules/flake/environments.nix`:

```nix
_: {
  environments.niri = {
    nixos = { pkgs, ... }: { programs.niri.enable = true; };  # body only, like features
    # homeManager = ...;                                      # optional
  };
}
```

A host picks its environments in `generic.<hostname>`:

```nix
hosts.<hostname>.environments = {
  primary = "hyprland";   # what the host boots into by default (null = none)
  gnome.enable = true;    # every other enabled environment becomes a specialisation
  plasma6.enable = true;
};
```

How it fits together:
- `nixos.specialisations` (imported by `nixos.base`) imports every `modules.nixos.environments.<name>` and declares `environments.active`, which defaults to `primary`. Each NixOS body is gated on `environments.active == <name>`.
- Each enabled non-primary environment becomes `specialisation.<name>` with `inheritParentConfig = true` and only `environments.active` forced to `<name>`, so host files carry no specialisation plumbing. nixpkgs drops nested specialisations itself.
- home-manager bodies are shared across specialisations, so they're gated on the host using the environment at all (`primary` or `.enable`).
- A body's `imports` are hoisted out of the gate, since imports can't be conditional (e.g. niri imports the Look NixOS module unconditionally; it only acts when `programs.lookapp.enable` is set).
- `primary` is reserved and can't be used as an environment name.

### Hosts

| Hostname | Type | Notes |
|----------|------|-------|
| edgenix | NixOS x86_64 | k3s node, Plasma6 + specialisations |
| inspernix | NixOS x86_64 | |
| nasnix | NixOS x86_64 | NAS + k3s node |
| nixmini | NixOS x86_64 | k3s node |
| powerspecnix | NixOS x86_64 | |
| steamdeck | home-manager only | user: deck |
| vavirl-pw0bwnq8 | home-manager only | WSL, user: drenfer (NixOS/WSL build currently disabled in `nixosConfigurations.nix`) |
| pixel8 | generic config only | Android phone; identity: duck; only feature flags + syncthing, no `nixosConfigurations`/`homeConfigurations` entry |

### Adding a New Host

Four files must be updated when adding a NixOS host. Missing any one causes evaluation errors (e.g. `attribute '<hostname>' missing`).

1. **`modules/hosts/<hostname>.nix`** — create the host file with three modules:
   - `generic.<hostname>` — feature flags, identity, syncthing shares, pubkey, Syncthing device ID
   - `homeManager.<hostname>` — extra packages, sessionPath
   - `nixos.<hostname>` — hardware config (UUIDs, kernel modules, CPU type), boot loader, timezone, specialisations

2. **`modules/hosts.nix`** — add `<hostname>` to the `imports` list inside `generic.hosts`. This is what makes `config.hosts.<hostname>` available everywhere (home-manager, NixOS, etc.). **Forgetting this causes the `attribute '<hostname>' missing` error.**

3. **`modules/flake/nixosConfigurations.nix`** — add `<hostname> = linux "<hostname>";` (or `wsl`/`linux-arm` as appropriate).

4. **`modules/flake/homeConfigurations.nix`** — add a `"<user>@<hostname>"` entry importing `[base <hostname>]` from `homeManager`.

A host that isn't built by Nix at all (e.g. `pixel8`, an Android phone tracked only for feature flags/syncthing) only needs steps 1–2 — skip the `nixosConfigurations`/`homeConfigurations` entries.

### Secrets

Managed via [sops-nix](https://github.com/Mic92/sops-nix). Secret files live in `secrets/`. GPG keys are used for encryption (`nur secrets list-keys`).

### Nushell

`nushell/` contains Nushell shell configuration (`config.nu`, `env.nu`) and custom modules/completions.

### Task Runner (`nur`)

[nur](https://github.com/nur-taskrunner/nur) is the task runner using Nushell. Tasks are defined in `scripts/nur.nu` as a Nushell module with `export def "nur <task>"` commands. **`scripts/nur.nu` is not deployed to systems** — it's local to this repo only.

`nurfile` (at repo root) simply does `overlay use scripts/nur.nu` to load the module. Tasks run with CWD as the repo root.
