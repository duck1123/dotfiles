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

Top-level registry directories (auto-loaded, see Registries):
- `features/` — features enabled/disabled per host (bluetooth, hyprland, kubernetes, etc.), one `<name>.nix` per feature (see Feature System)
- `environments/` — desktop environments (budgie, gnome, hyprland, i3, niri, plasma6), one `<name>.nix` per environment (see Environment System)
- `identities/` — per-user identities (duck, deck, drenfer)
- `hosts/` — one `<hostname>.nix` per host (see Host Configuration Pattern)

Key subdirectories of `modules/`:
- `modules/flake/` — flake outputs: `nixosConfigurations`, `homeConfigurations`, `devShells`, `packages`, and the `lib/+mk-os.nix` helpers
- `modules/nixos/` — NixOS-specific modules (boot, users, i18n, sddm, etc.)
- `modules/options/` — NixOS option declarations (host, hosts, identities, simpleFeature type)
- `modules/types/` — custom Nix types/submodules for hosts, identities, features
- `modules/base.nix`, `modules/state-version.nix` — the `homeManager.base`/`nixos.base` entry modules and state versions
- `modules/kubernetes/` — the k3s fleet-ops integration, fully consolidated into this repo (the `k3s-fleetops` flake input is gone; there is no external dependency left). `_vendor/applications/` and `_vendor/generators/`/`_vendor/lib/`/`_vendor/modules/` hold the application library/generators (edit these directly — "_vendor" is a historical name, not a sync boundary); `_env/dev/` has the per-app instance config for the `dev` nixidy environment. See `modules/kubernetes/docs/` for deployment workflow, the two-module-system gotcha, pinned-volume handling, and a troubleshooting playbook.

### Host Configuration Pattern

Each host is one file, `hosts/<hostname>.nix`, in the `hosts` registry (`modules/flake/hosts.nix`). It holds the host's data (the `hostSubmodule` options) plus its own modules:

```nix
# hosts/edgenix.nix
{ config, ... }:            # flake-parts args, for config.identities
{
  system = "x86_64-linux";  # hostname defaults to the file name, name to hostname
  identity = config.identities.duck;
  id = "...";               # Syncthing device ID
  environments.primary = "plasma6";
  features = { git.enable = true; ... };
  nixos.enable = true;      # build nixosConfigurations.<hostname>

  modules.homeManager = { pkgs, ... }: { home.packages = [ pkgs.guake ]; };
  modules.nixos = { config, lib, modulesPath, ... }: { ... };  # hardware, boot, imports nixos.base
  # homeConfigurationName = "user@HOST";  # default "<identity.username>@<hostname>"
}
```

From that the registry generates:
- `inputs.self.hosts.<hostname>` (the data, evaluated once), exposed read-only to generic/NixOS/home-manager modules as `config.hosts` (`modules/options/hosts-options.nix`)
- `modules.homeManager.<hostname>` / `modules.nixos.<hostname>` (published unwrapped, so merge order matches a hand-written module)
- `homeConfigurations.<homeConfigurationName>` for every host with `modules.homeManager` (`[ base <hostname> ]`, plus `host = config.hosts.<hostname>`)
- `nixosConfigurations.<hostname>` for every host with `modules.nixos` and `nixos.enable`, via `mkNixos` from `modules/flake/lib/+mk-os.nix`, plus `host = config.hosts.<hostname>`. A WSL host imports `inputs.self.modules.nixos.wsl` from its own `modules.nixos` (see `vavirl-pw0bwnq8`).

Host modules don't set `host` themselves; anything else importing `modules.<class>.<hostname>` has to.

### Feature System

Features are toggled with `enable = true/false` under `hosts.<hostname>.features.<name>`.

Each feature is declared **once**, in `features/<name>.nix`, which holds the body of `features.<name>` in the registry defined in `modules/flake/features.nix`. That single declaration generates:
- the `hosts.<host>.features.<name>` option (via `simpleFeatureWith` in `modules/options/simpleFeature.nix`)
- `modules.homeManager.features.<name>` and `modules.nixos.features.<name>`, each wrapped in `mkIf hosts.<host>.features.<name>.enable`
- their inclusion in `homeManager.base` / `nixos.base` (which import every registered feature; there is no list to edit)

```nix
# features/vim.nix
{
  # description = "...";            # optional; defaults to "<name> feature"
  homeManager = { pkgs, ... }: { home.packages = [ pkgs.neovim ]; };  # body only: no `config =` / `mkIf`
  nixos = _: { programs.vim.enable = true; };                         # optional, either class may be omitted
}
```

Registry knobs beyond the two bodies (a body may also be a list of bodies):
- `extraOptions = { inputs, lib }: { foo = mkOption ...; };` — extra options next to `enable` (see `tailscale`, `nix`, `hyprland`)
- `option = { inputs, lib }: mkOption ...;` — replace the whole generated option with a custom type (see `media`, `kubernetes`)
- `gated = false` — skip the automatic `enable` gate when the feature has no plain `enable` or gates differently per class; the bodies then return their own `mkIf`/`mkMerge` (see `media`, `kubernetes`)

Gotchas:
- flake-parts types `flake.modules.<class>.<name>` as a `deferredModule`, so `features.<name>` can't be nested there. The registry sets `nestedModules.<class>.features` (`modules/flake/nested-modules.nix`), which is written into the *published* flake output through `touchup.attr.modules.finish`, so it is visible as `inputs.self.modules.<class>.features.<name>` but **not** in `config.flake.modules` inside flake-parts modules. `finish` only takes one definition, so any other nested group must go through `nestedModules` too.
- A body that sets options which only exist in some hosts (e.g. `sops.*`) can't live in a feature that WSL also imports, because `mkIf false` still errors on an undeclared option. That is why `modules/nixos/nix-attic.nix` stays a standalone `modules.nixos.nix-attic` imported directly by `nixos.base`.
- Non-feature modules (`state-version`, `boot`, `i18n`, `users`, `sddm`, `environments-*`) are still plain `flake.modules.<class>.<name>` and are listed explicitly in `modules/base.nix`.

### Environment System

Desktop environments work like features. Each is declared once in `environments/<name>.nix`, which holds the body of `environments.<name>` in the registry in `modules/flake/environments.nix`:

```nix
# environments/niri.nix
{
  features = [ "wayle" ];      # features turned on for hosts using this environment
  desktopNames = [ "niri" ];   # XDG_CURRENT_DESKTOP of the session
  nixos = { pkgs, ... }: { programs.niri.enable = true; };  # body only, like features
  # homeManager = ...;                                      # optional
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
- `features` are set with `mkDefault true` on every host that uses the environment (primary or enabled), via `modules.generic.environments-host` in the host submodule. They remain ordinary features, so several environments can share one (hyprland and niri both pull in `wayle`) and a host can still set one directly or turn it off. Don't enable environment-owned features (`hyprland`, `gnome`, `i3`, `wayle`) by hand in host files.
- Because home-manager is shared across specialisations, a feature that runs a service should limit it to the environments that asked for it at runtime: `inputs.self.lib.environments.desktopsFor config.host "<feature>"` returns their `desktopNames` (see `wayle`, which uses them as `ConditionEnvironment=|XDG_CURRENT_DESKTOP=...` so it doesn't start under Plasma).
- `primary` is reserved and can't be used as an environment name.
- Anything consumed as a module (like `environments-host`) must be published under `flake.modules`, not `flake.types.generic`: that option is typed `anything`, which wraps function values and forces config-dependent definitions too early (infinite recursion).

### Registries (top-level directories)

Some core types live in top-level directories outside `modules/` and are auto-loaded by `modules/flake/registries.nix`: every `<dir>/<name>.nix` becomes a flake-parts definition of `<attr>.<name>`, so adding one means dropping in a file (files starting with `_` are skipped). A file holds only the entry's body, either a plain value or a function of flake-parts module args (`config`, `lib`, `inputs`, ...) when it needs them, e.g. to reference other entries. Note that such a function gets the *flake-parts* args, not NixOS/home-manager ones; those belong to the nested `nixos`/`homeManager` bodies. Relative paths are relative to the registry directory (e.g. `../resources/...`).

```nix
# identities/deck.nix
{ config, ... }: {
  inherit (config.identities.duck) email gpgKey name;   # flake-parts config
  username = "deck";
}
```

The registry's option is declared in `modules/flake/<attr>.nix` and evaluated once at the flake level. To add a new registry, add a `<attr> = ../../<dir>;` line to the table in `registries.nix` and declare the option.

Current registries:
- `features/`, `environments/`, `hosts/` — see Feature System, Environment System and Host Configuration Pattern.
- `identities/` — per-user identities (duck, deck, drenfer). Declared in `modules/flake/identities.nix`, published as `inputs.self.identities`, and exposed read-only to generic/NixOS/home-manager modules as `config.identities` (`modules/options/identities-options.nix`). Hosts pick one with `identity = config.identities.<name>`.

### Hosts

| Hostname | Type | Notes |
|----------|------|-------|
| edgenix | NixOS x86_64 | k3s node, Plasma6 + specialisations |
| inspernix | NixOS x86_64 | |
| nasnix | NixOS x86_64 | NAS + k3s node |
| nixmini | NixOS x86_64 | k3s node |
| powerspecnix | NixOS x86_64 | |
| steamdeck | home-manager only | user: deck |
| vavirl-pw0bwnq8 | NixOS-WSL + home-manager | WSL, user: drenfer; home config is `drenfer@VAVIRL-PW0BWNQ8` |
| pixel8 | generic config only | Android phone; identity: duck; only feature flags + syncthing, no `nixosConfigurations`/`homeConfigurations` entry |

### Adding a New Host

Create `hosts/<hostname>.nix` (copy an existing one); nothing else needs registering. Set `modules.nixos` + `nixos.enable = true` to get a `nixosConfigurations` entry and `modules.homeManager` (even `{ }`, see `steamdeck`) to get a `homeConfigurations` entry. A host that isn't built by Nix at all (e.g. `pixel8`, an Android phone tracked only for feature flags/syncthing) sets neither.

`scripts/nur.nu` still has its own hardcoded host lists (`nixos-hosts`, `home-hosts`, `host-flake-name`, `host-user`) for tab completion; add the host there too.

### Secrets

Managed via [sops-nix](https://github.com/Mic92/sops-nix). Secret files live in `secrets/`. GPG keys are used for encryption (`nur secrets list-keys`).

### Nushell

`nushell/` contains Nushell shell configuration (`config.nu`, `env.nu`) and custom modules/completions.

### Task Runner (`nur`)

[nur](https://github.com/nur-taskrunner/nur) is the task runner using Nushell. Tasks are defined in `scripts/nur.nu` as a Nushell module with `export def "nur <task>"` commands. **`scripts/nur.nu` is not deployed to systems** — it's local to this repo only.

`nurfile` (at repo root) simply does `overlay use scripts/nur.nu` to load the module. Tasks run with CWD as the repo root.
