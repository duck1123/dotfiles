# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Overview

A Nix flake that manages several NixOS hosts, their home-manager configs and a k3s cluster. It uses [flake-parts](https://github.com/hercules-ci/flake-parts) + [import-tree](https://github.com/vic/import-tree) to auto-import every module under `./modules/`. Tasks run through [nur](https://github.com/nur-taskrunner/nur) (Nushell), defined in `scripts/nur.nu`.

Read the page for the area you're touching before changing it:

| Page | Read it when |
|---|---|
| [docs/nur_commands.md](docs/nur_commands.md) | Running any `nur` task you haven't used in this session |
| [docs/architecture.md](docs/architecture.md) | Editing hosts, features, environments, identities or any registry, or `modules/flake/` |
| [docs/secrets.md](docs/secrets.md) | Touching `secrets/`, `.sops.yaml` or anything that reads a secret |
| [modules/kubernetes/docs/](modules/kubernetes/docs/) (start with `architecture.md`) | Anything in `appTemplates/`, `clusters/`, `modules/kubernetes/` or the live cluster |
| [docs/setup.md](docs/setup.md) | Setting up a new machine |

## Common commands

```sh
nur build [os|home|k8s]       # build this host's configs (k8s = render manifests only)
nur switch [os|home]          # apply locally; --host <h> to deploy to another host
nur check                     # nix flake check
nur format                    # nixfmt every .nix file
nur lint                      # statix
```

## Rules

- **Never put a secret value into the conversation.** Don't run `sops -d` or `nur secrets decrypt`, and don't read `secrets/k8s.yaml`. Find a path with `nur secrets keys`, check it with `nur secrets check`, and use it via `nur secrets exec VAR=path sh -c '... "$VAR" ...'`, which redacts values in the output. Never pass `--no-redact`.
- **`nur switch k8s`, `nur k8s deploy` and `nur k8s push` deploy to the cluster.** Confirm before running them, and pass `--no-review` (there's no terminal). Use `nur k8s switch-charts` to render manifests without deploying.
- **`git add` new files.** The flake only sees git-tracked files; an untracked `hosts/`, `features/`, `appTemplates/` (etc.) file is silently ignored (`does not provide attribute ...`). Staging is enough.
- **Feature and environment files hold bodies only.** A `features/<name>.nix` `homeManager`/`nixos` body has no `config =` or `mkIf enable` wrapper; the registry adds the gate. Don't enable environment-owned features (`hyprland`, `gnome`, `i3`, `wayle`) by hand in host files.
- When adding or changing a `nur` task, update [docs/nur_commands.md](docs/nur_commands.md) to match. `scripts/nur.nu` is local to this repo; it isn't deployed to hosts.

## Hosts

Hosts are `hosts/<hostname>.nix`; the [README](README.md#hosts) has the table and `nix eval --json .#hostInfo` has the details. Non-obvious ones:

- `vavirl-pw0bwnq8` is NixOS-WSL with user `drenfer`. Its home config is `drenfer@VAVIRL-PW0BWNQ8`, and `--host` takes `vallen` (its `sshHost`).
- `steamdeck` is home-manager only (user `deck`).
- `pixel8` is an Android phone: feature flags and Syncthing only, with no build target.
