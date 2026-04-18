# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Overview

This is a Nix flake-based dotfiles/system configuration repo managing multiple NixOS hosts and home-manager configurations. It uses [flake-parts](https://github.com/hercules-ci/flake-parts) + [import-tree](https://github.com/vic/import-tree) to auto-import all modules from `./modules/`.

The primary task runner is [Babashka](https://github.com/babashka/babashka) (`bb`), with tasks defined in `bb.edn`.

## Key Commands

```sh
# Build and apply local configuration
bb build           # build current home + OS configs
bb switch          # switch both home-manager and NixOS
bb switch-home     # apply home-manager config only
bb switch-os       # apply NixOS config only
bb boot-os         # build NixOS and set as boot default (safe for slow activations)

# Validation and formatting
bb check           # run nix flake check
bb format          # format all .nix files with nixfmt

# Remote deployment (builds locally, copies + activates remotely)
bb switch-remote-edgenix   # deploy to edgenix
bb switch-remote-nasnix    # deploy to nasnix
bb diff-remote-os-edgenix  # show package changes before deploying

# Flake maintenance
nix flake update   # update flake.lock
bb build-all       # build all configurations (nom build .#ci)
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
- `modules/features/` — feature modules enabled/disabled per host (bluetooth, hyprland, kubernetes, etc.)
- `modules/environments/` — desktop environment modules (gnome, hyprland, i3, plasma6, budgie)
- `modules/nixos/` — NixOS-specific modules (boot, users, i18n, sddm, etc.)
- `modules/options/` — NixOS option declarations (host, hosts, identities, simpleFeature type)
- `modules/types/` — custom Nix types/submodules for hosts, identities, features
- `modules/identities/` — per-user identity definitions (duck, deck, drenfer)

### Host Configuration Pattern

Each host file (e.g., `modules/hosts/edgenix.nix`) defines three namespaced modules in `flake.modules`:
1. `generic.<hostname>` — shared config: features enabled/disabled, identity assignment, syncthing shares
2. `homeManager.<hostname>` — home-manager extras: extra packages, session paths
3. `nixos.<hostname>` — NixOS hardware config + specialisations (multiple DE variants via `specialisation`)

`modules/flake/nixosConfigurations.nix` builds hosts using helpers from `lib/+mk-os.nix`:
- `linux "hostname"` → `nixpkgs.lib.nixosSystem` with `modules.nixos.base` + `modules.nixos.<hostname>`
- `wsl "hostname"` → WSL variant

### Feature System

Features are toggled with `enable = true/false` under `hosts.<hostname>.features.<name>`. The `simpleFeature` type (in `modules/options/simpleFeature.nix`) provides the standard `{ enable = false; }` submodule pattern. Some features have richer submodules (e.g., `kubernetes`, `syncthing`, `media`).

`modules/features/base.nix` defines what's included by default in both `homeManager.base` and `nixos.base` — all feature modules are imported here, then individually toggled per-host.

### Hosts

| Hostname | Type | Notes |
|----------|------|-------|
| edgenix | NixOS x86_64 | Primary desktop, Plasma6 + specialisations |
| inspernix | NixOS x86_64 | |
| nasnix | NixOS x86_64 | NAS + k3s server |
| nixmini | NixOS x86_64 | |
| powerspecnix | NixOS x86_64 | |
| vidcentre | NixOS x86_64 | |
| steamdeck | home-manager only | user: deck |
| vavirl-pw0bwnq8 | home-manager only | WSL, user: drenfer |

### Adding a New Host

Four files must be updated when adding a NixOS host. Missing any one causes evaluation errors (e.g. `attribute '<hostname>' missing`).

1. **`modules/hosts/<hostname>.nix`** — create the host file with three modules:
   - `generic.<hostname>` — feature flags, identity, syncthing shares, pubkey, Syncthing device ID
   - `homeManager.<hostname>` — extra packages, sessionPath
   - `nixos.<hostname>` — hardware config (UUIDs, kernel modules, CPU type), boot loader, timezone, specialisations

2. **`modules/hosts.nix`** — add `<hostname>` to the `imports` list inside `generic.hosts`. This is what makes `config.hosts.<hostname>` available everywhere (home-manager, NixOS, etc.). **Forgetting this causes the `attribute '<hostname>' missing` error.**

3. **`modules/flake/nixosConfigurations.nix`** — add `<hostname> = linux "<hostname>";` (or `wsl`/`linux-arm` as appropriate).

4. **`modules/flake/homeConfigurations.nix`** — add a `"<user>@<hostname>"` entry importing `[base <hostname>]` from `homeManager`.

5. **`bb.edn`** — add the corresponding bb tasks:
   - `build-home-<hostname>`, `build-os-<hostname>`
   - `build-remote-os-<hostname>`, `build-remote-home-<hostname>`
   - `diff-remote-os-<hostname>`, `dry-run-remote-os-<hostname>`
   - `switch-remote-<hostname>`, `switch-remote-os-<hostname>`, `switch-remote-home-<hostname>`

### Secrets

Managed via [sops-nix](https://github.com/Mic92/sops-nix). Secret files live in `secrets/`. GPG keys are used for encryption (`bb list-secret-keys`).

### Nushell

`nushell/` contains Nushell shell configuration (`config.nu`, `env.nu`) and custom modules/completions.
