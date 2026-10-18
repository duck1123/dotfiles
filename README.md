# Duck's dotfiles

This is a single Nix flake that defines an entire personal computing fleet — every NixOS machine, every home-manager profile, and the Kubernetes cluster they all talk to — as one evaluated, reproducible whole. There's no separate "dotfiles" repo plus a separate "infra" repo plus a separate "k8s" repo drifting out of sync with each other; a change here can touch a laptop's window manager config, a server's systemd units, and a cluster app's ingress domain, and all of it is checked, built, and applied through the same handful of commands.

The flake is assembled with [flake-parts](https://github.com/hercules-ci/flake-parts) and [import-tree](https://github.com/vic/import-tree), which auto-imports every module under `./modules/` — adding a new host, feature, or identity is mostly a matter of dropping a file in the right place rather than wiring it into a central list by hand. Everything is driven through one task runner, [nur](https://github.com/nur-taskrunner/nur) (Nushell-based, tasks defined in `scripts/nur.nu`), so `nur switch`, `nur build`, `nur check` and friends work the same way whether the target is a NixOS host, a home-manager profile, or the Kubernetes cluster.

### What's in here

- **NixOS + home-manager** configs for every machine in the fleet, built from per-host files that toggle shared features and desktop environments.
- **Kubernetes fleet management**: application definitions, environment config, secrets, and the automation to build and deploy manifests via ArgoCD/nixidy. This used to live in a separate repo (`k3s-fleetops`); it's since been folded in here so the whole fleet — machines and cluster alike — is defined in one place.
- **Secrets** encrypted with [sops](https://github.com/getsops/sops) and age keys, covering both host-level (via [sops-nix](https://github.com/Mic92/sops-nix)) and cluster secrets.
- **Nushell configuration** (`nushell/`) shared across every machine, plus the `nur` task definitions that drive everything above.

## Hosts

| Hostname | Type | Description |
|----------|------|-------------|
| edgenix | NixOS x86_64 | k3s node, Plasma6 + specialisations |
| inspernix | NixOS x86_64 | Laptop |
| nasnix | NixOS x86_64 | Virtualized server on NAS, k3s node |
| nixmini | NixOS x86_64 | k3s node |
| powerspecnix | NixOS x86_64 | Primary gaming PC |
| steamdeck | home-manager only | user: deck |
| vavirl-pw0bwnq8 | NixOS-WSL + home-manager | user: drenfer; home config `drenfer@VAVIRL-PW0BWNQ8`, `--host vallen` |
| pixel8 | feature/syncthing config only | Android phone; no NixOS or home-manager build target |

`hosts/<hostname>.nix` is the source of truth; `nix eval --json .#hostInfo` prints every host's details.

## Usage

Day-to-day work is all `nur` tasks, run from anywhere in the repo:

```sh
nur switch                  # apply home-manager + NixOS on this machine
nur switch --host edgenix   # build locally, deploy to another host
nur switch k8s              # build and deploy the cluster manifests
nur check                   # nix flake check
```

## Layout

```
dotfiles/
  hosts/                   ← one file per machine: identity, features, environments, host modules
  features/                ← shared features hosts toggle (one file each)
  environments/            ← desktop environments (one file each)
  identities/              ← per-user identities
  modules/                 ← flake-parts modules (auto-imported): flake outputs, NixOS modules, k8s library
  appTemplates/            ← Kubernetes app definitions (one mkArgoApp spec per app)
  clusters/dev/            ← the dev cluster: cluster-wide settings and per-app config
  resources/apps/          ← app-owned assets (dashboards, scripts, site sources)
  secrets/                 ← sops-encrypted secrets
  kubernetes/
    infra-manifests/       ← ArgoCD bootstrap manifests (install, 00-master app-of-apps)
    manifests/             ← checkout of argo-manifests (git-ignored here)
  nushell/                 ← Nushell config, modules and completions
  scripts/nur.nu           ← nur task definitions
```

## Documentation

| Page | Covers |
|---|---|
| [docs/setup.md](docs/setup.md) | Setting up a new machine, including the NixOS-WSL image |
| [docs/nur_commands.md](docs/nur_commands.md) | Every `nur` task: build/switch, secrets, k8s deploy, cluster operations |
| [docs/architecture.md](docs/architecture.md) | How the flake fits together: registries, hosts, features, environments |
| [docs/secrets.md](docs/secrets.md) | sops/age setup, who can decrypt what, adding recipients |
| [modules/kubernetes/docs/](modules/kubernetes/docs/) | The cluster: architecture, deployment workflow, [bootstrapping a new cluster](modules/kubernetes/docs/bootstrap.md), pinned volumes, troubleshooting |
| [CLAUDE.md](CLAUDE.md) | Orientation and rules for AI agents working in this repo |
