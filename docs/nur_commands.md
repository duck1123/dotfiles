# nur commands

Every task in this repo runs through [nur](https://github.com/nur-taskrunner/nur), a Nushell-based task runner. This page is the reference for those tasks: what each one does, its flags, and the gotchas worth knowing before you run it.

## How it works

- Tasks are `export def "nur <task>"` commands in `scripts/nur.nu`. The `nurfile` at the repo root just does `overlay use scripts/nur.nu`.
- `nur` finds the `nurfile` from the repo root or any subdirectory, and tasks always run with the repo root as CWD.
- `scripts/nur.nu` is local to this repo; it isn't deployed to any host.
- `nur --help` lists every task with its doc comment. Nushell tab completion works for hosts (`--host`), targets (`os`/`home`/`k8s`), secret files and app names.

### Getting `nur`

On hosts with the `nushell` feature, `nur` is already on `PATH`. Before home-manager has run, use `nix run .#pnu` or the devShell (see [setup.md](setup.md#3-tools-nur-and-the-devshell)).

### Hosts and `--host`

Host lists and `--host` resolution come from `nix eval --json .#hostInfo` (the working tree), so a new `hosts/<name>.nix` is picked up automatically once it's `git add`ed. `--host` takes a host's `sshHost` (e.g. `vallen` for `vavirl-pw0bwnq8`) or its hostname, and maps it to that host's `homeConfiguration` / `nixosConfiguration`.

## Build and switch

Both commands take positional targets `os`, `home` and `k8s`. With no target they act on `os` and `home`; `k8s` is opt-in only. On a host that isn't NixOS (e.g. steamdeck, vallen), a plain local `nur build`/`nur switch` quietly skips `os`; naming `os` explicitly, or passing `--host`/`--all`, makes it fail instead.

### `nur build`

```sh
nur build                      # build this host's os + home configs
nur build home                 # only home-manager
nur build home k8s             # home-manager + k8s manifests
nur build --host edgenix       # build another host's configs (no activation)
nur build --all                # build every host's os + home configs
nur build --tarball --host vavirl-pw0bwnq8   # NixOS-WSL tarball builder (see setup.md)
```

- All selected os/home outputs are built as one `nom` graph, so progress shows in one place. Locally, `nh home build` / `nh os build` then run on top to print package diffs.
- `--fallback` passes `--fallback` to Nix (build locally when a substituter fails, e.g. a flaky Attic).
- `k8s` runs `nur k8s switch-charts`: it renders manifests into `kubernetes/manifests/` but deploys nothing. It can't be combined with `--host`.
- `--all` and `--host` are mutually exclusive, as are `--tarball` and `--all`.

### `nur switch`

```sh
nur switch                     # home-manager + NixOS on this host
nur switch home                # home-manager only
nur switch os                  # NixOS only
nur switch os --boot           # make the new NixOS generation the boot default without activating it
nur switch --host edgenix      # build locally, copy to edgenix, activate home + os there
nur switch --host edgenix os   # same, NixOS only
nur switch k8s                 # build manifests, review in magit, confirm, push = deploy
nur switch k8s --no-review     # same without the review/confirmation
```

- Locally, `home` builds with `nom` first, then runs `home-manager switch` with a timestamped backup extension (`-b backup.<epoch>`). `os` builds with `nom`, then `sudo nixos-rebuild switch` (or `boot`). If the switch fails, the last 50 journal lines are printed.
- With `--host`, everything is built locally, `nix copy`'d over SSH and activated remotely. Activating `os` prompts for the remote sudo password. Prerequisites: key-based SSH to the host, and sudo there.
- `--boot` only applies to the `os` target. Use it when activation itself is slow or risky (e.g. a kernel or k3s change on a cluster node).
- `k8s` is the same as `nur k8s deploy` (see [Kubernetes manifests](#kubernetes-manifests)). It **deploys to the cluster**, so it can't be combined with `--host`.

### Other NixOS helpers

| Command | What it does |
|---|---|
| `nur diff-os --host <h>` | Build `<h>`'s system locally and `nix store diff-closures` it against what's running there. `--host` is required. |
| `nur dry-run-os [--host <h>]` | `nixos-rebuild dry-activate`, locally or against a remote host. |
| `nur bootstrap-os` | First switch on a fresh NixOS install where `nix-command`/`flakes` aren't enabled yet (`nh os boot .` with them forced on). |

## Validation and formatting

| Command | What it does |
|---|---|
| `nur check` | `nix flake check` |
| `nur ci` | `nur check` followed by `nur build --all` |
| `nur format` | Run `nixfmt` on every `.nix` file |
| `nur lint` / `nur lint nix` | `statix check .` |
| `nur lint nushell` | `nu-lint`, failing on warnings as well as errors. Not part of `nur lint`/`nur ci` yet because of existing warnings in `scripts/nur.nu`. |

Flake inputs are updated with plain `nix flake update`; there's no task for it.

## Secrets

Secrets are encrypted with sops and age keys; [secrets.md](secrets.md) covers the files, who can decrypt them and how to add a key. `secrets/k8s.enc.yaml` is the default file for every task below. Pass another file with `--file` for `keys`/`check`/`exec`, or as a positional argument for the rest.

```sh
nur secrets edit [file]          # edit in $EDITOR; no plaintext ever touches disk
nur secrets decrypt [file]       # secrets/k8s.enc.yaml -> secrets/k8s.yaml (git-ignored)
nur secrets encrypt [file]       # secrets/k8s.yaml -> secrets/k8s.enc.yaml with the right creation rule
nur secrets keys [prefix]        # list dotted key paths, e.g. postgresql.adminPassword (no decryption)
nur secrets check <path>...      # set? length + sha256 prefix, never the value
nur secrets exec VAR=path... cmd [args...]   # run cmd with secrets as env vars, output redacted
nur secrets windows-key          # read the Windows product key from ACPI firmware (sudo)
```

- `edit` is the normal way to change a secret. `decrypt`/`encrypt` only work on `*.enc.*` files and refuse to write a plaintext path that isn't git-ignored. Delete the plaintext once you've re-encrypted it.
- `exec` maps each leading `VAR=dotted.path` to an env var, then runs the rest as the command. Any secret value in its stdout/stderr comes out as `[REDACTED:VAR]`. Reference the variable inside the command, not on the outer command line:

  ```sh
  nur secrets exec PGPASSWORD=postgresql.adminPassword sh -c 'psql -h db -U postgres -c "\l"'
  nur secrets exec --file secrets/mcp.yaml TOKEN=trilium_token sh -c 'curl -H "Authorization: $TOKEN" ...'
  ```

  `--no-redact` streams output (and stdin) unfiltered for interactive use.
- Changing a secret's value updates the k8s Secret on the next deploy, but pods read `secretKeyRef` env vars at startup, so run `nur apps restart <name>` afterwards.

> **Agents:** never put a secret value into the conversation; see [secrets.md](secrets.md#rules-for-agents).

## Kubernetes manifests

App definitions live in `appTemplates/` and `clusters/dev/`. nixidy renders them into `kubernetes/manifests/`, a checkout of the private `argo-manifests` repo, which ArgoCD syncs from. Nothing reaches the cluster until those manifests are pushed. See [deployment-workflow.md](../modules/kubernetes/docs/deployment-workflow.md) for the gotchas, and [bootstrap.md](../modules/kubernetes/docs/bootstrap.md) for setting up a new cluster.

| Command | What it does |
|---|---|
| `nur k8s deploy [--no-review]` | `manifests sync` + `switch-charts`, then (unless `--no-review`) opens the changes in magit and asks before running `push` (the prompt starts on Abort, so a bare Enter doesn't deploy). **Deploys.** `nur switch k8s` calls this. |
| `nur k8s switch-charts [--fallback]` | Render manifests into `kubernetes/manifests/dev/` (plus sops secrets, post-processing, and pushing pinned store paths to Attic). Local only; inspect with `git -C kubernetes/manifests diff`. |
| `nur k8s push` | Commit and push whatever changed in `kubernetes/manifests/`. **This is the deploy step.** |
| `nur k8s manifests init` | One-time: clone `argo-manifests` into `kubernetes/manifests/`. |
| `nur k8s manifests sync` | `git pull` the manifests checkout. |
| `nur k8s generate-deploy-key` | Generate an SSH deploy key pair for `argo-manifests`. |
| `nur k8s bootstrap-argocd-repo` | One-time: apply ArgoCD's repo credential for `argo-manifests` from secrets. |

- The review step needs a terminal and blocks until you close the emacs frame (`C-x 5 0`). If you abort, the changes stay uncommitted in `kubernetes/manifests/`; push them later with `nur k8s push`. If nothing changed, it says so and stops.
- The dotfiles source change still needs committing to this repo separately.

> **Agents:** `nur switch k8s` / `nur k8s deploy` / `nur k8s push` deploy to the cluster, so confirm before running them, and pass `--no-review` (there's no terminal). Use `nur k8s switch-charts` when you only need to see the rendered diff.

## Cluster operations

### Apps

| Command | What it does |
|---|---|
| `nur apps list` | Every app name (from `appTemplates/` and `modules/kubernetes/nixidy-apps/`). |
| `nur apps restart <name>` | `kubectl rollout restart` the Deployment named `<name>` in namespace `<name>` (falling back to a StatefulSet) and wait for it. Apps with another namespace or several workloads (Helm charts, nix-csi) need a manual `kubectl` restart. |

To scale or pause an app, change `replicas` in `clusters/dev/apps/<name>.nix` and deploy. `kubectl scale` gets reverted by ArgoCD's selfHeal.

### ArgoCD

| Command | What it does |
|---|---|
| `nur argocd refresh [name]` | Make ArgoCD re-diff against git now instead of waiting for its poll interval. No name = every Application. Doesn't apply anything, run hooks, or retry a failed operation. |
| `nur argocd sync [name]` | Start a real sync (applies changes, runs `Sync`/`PostSync` hooks). Uses `argocd --core` against a temporary kubeconfig set to the `argocd` namespace, so no port-forward or login is needed. No name = every Application. |
| `nur argocd install` | Install or upgrade ArgoCD from `kubernetes/infra-manifests/argocd/` (downloads `install.yaml` first if missing). Safe to re-run. |
| `nur argocd update-manifest` | Download the latest stable ArgoCD `install.yaml` into `kubernetes/infra-manifests/argocd/`; commit it to pin the version. |
| `nur argocd apply-master` | Apply `kubernetes/infra-manifests/00-master.yaml`, the app-of-apps that tracks `dev/apps` in `argo-manifests`. Only needed when bootstrapping a cluster that doesn't have `00-master` yet. |

### Port-forwards

| Command | What it does |
|---|---|
| `nur forward argocd` | ArgoCD UI on https://localhost:8080 |
| `nur forward traefik` | Traefik dashboard on http://localhost:9000 |

### Databases

| Command | What it does |
|---|---|
| `nur postgres list` | Databases and their sizes. |
| `nur postgres backup [--database db] [--output-dir dir]` | `pg_dump` every database (or one) to `./backups/postgresql/` as both `<db>_<timestamp>.sql.gz` and `.custom`. |
| `nur postgres list-backups` | List the nightly dumps in the NAS's `DatabaseBackups/postgresql` (via a throwaway pod). |
| `nur postgres restore <file> [--database db] [--recreate]` | Restore a local `.sql`, `.sql.gz` or `.custom` file. The target database is inferred from a `<db>_<YYYYMMDD>_<HHMMSS>` filename unless `--database` is given; `--recreate` drops it first. A bare filename that doesn't exist locally restores that dump from the PVC via a Job. |
| `nur mariadb list-backups` | List the nightly dumps in the NAS's `DatabaseBackups/mariadb`. |
| `nur mariadb restore [--backup-file f]` | Restore a dump from the PVC via a Job (prompts with the list if no file is given). **Replaces all existing databases.** |
| `nur sqlite list <ns>` | SQLite databases (found by file header) on the namespace's Longhorn PVCs, with sizes. NFS PVCs are skipped. `excluded` marks the ones `backup` skips by default. |
| `nur sqlite backup <ns> [--pvc p] [--exclude re] [--output-dir dir]` | Consistent online copy of each database (`sqlite3 .backup`, then `integrity_check`), gzipped to `./backups/sqlite/<ns>/<timestamp>/<pvc>/<path>.gz`. Safe while the app runs: a throwaway `sqlite-helper-*` pod mounts the PVCs on the app's node. `--exclude` defaults to paths under a `backup/` or `backups/` dir; `""` keeps everything. |
| `nur sqlite restore <dir-or-file>` | Restore a whole backup run directory or one `.gz` from it, inferring namespace, PVC and path from the layout. Refuses while any pod mounts the PVC: set the app's replicas to 0 and deploy first (not `kubectl scale`). Backs the current databases up to `<timestamp>-pre-restore/` first, keeps file owner and mode, and deletes stale `-wal`/`-shm`. |

### Longhorn cold archive

Copies detached Longhorn volumes (apps that are off but whose data you want to keep) to the NAS's `ClusterBackups` share in two forms. A **block backup** goes to `backupstore/`, the cluster's Longhorn backup target, for an exact restore: restore it into a new volume and re-enable the app. A **file export** goes to `exports/<ns>/<pvc>/`, with `data/`, `MANIFEST.sha256` and `INFO`, for browsing or copying files out. Neither task ever deletes the source volume.

| Command | What it does |
|---|---|
| `nur longhorn cold-archive <ns>/<pvc> \| <ns> \| <volume> [--as <ns>/<pvc>]` | For each matching detached volume: snapshot plus Longhorn backup (`cold-<id>`), a read-only file copy checked against a sha256 manifest, then a restore of the block backup into a scratch volume checked against the same manifest, owners and modes. Labels the Backup `cold-verified=true`. Scratch PVs/PVCs/pods go in the `volume-export` namespace and are removed afterwards. A bare `<ns>` takes all its detached volumes. Two volumes sharing a PVC name get `-<id>` added to their export folder. `--as` names the export for a volume given by name whose PVC Longhorn no longer records. |
| `nur longhorn cold-list` | Detached volumes with their app's `enable` state in `clusters/dev/apps`, and their cold backup's state and whether the restore check passed. |

### NAS archive (RECOVERY_BACKUP drive)

Copies folders from the NAS's `/volume1` onto the RECOVERY_BACKUP USB drive attached to it, at the same path (`TV/Looney Tunes` → `<drive>/TV/Looney Tunes`). Jobs run on the NAS over SSH as the systemd unit `nas-archive-<path>`, so they survive this machine sleeping. Every job's disk reads and writes on `/volume1` and the drive are capped at `--bwlimit` (default 60M; `0` = none) through the unit's cgroup, which throttles hashing as well as the copy: an uncapped hash of the drive hung the whole NAS. `scripts/nas-archive.sh` is reinstalled to `/usr/local/bin/nas-archive` on every run. Each folder's sha256 manifest stays on the drive as `.archive.sha256`. Every outcome is appended to `ARCHIVE.log` at the drive's root.

| Command | What it does |
|---|---|
| `nur nas archive <share/folder> [--dry-run] [--skip-arr] [--bwlimit 60M]` | Copy (resumable rsync), hash the source, then check the drive copy against it with the page cache dropped. **Doesn't delete the source**: a verified run prints the `rm` command. Re-running resumes. For a `TV/<show>` or `Movies/<movie>` folder it first sets `Monitored = false` in Sonarr's or Radarr's database, and refuses while that app is running (unmonitor in its UI, then pass `--skip-arr`). |
| `nur nas archive-verify <share/folder> [--bwlimit 60M]` | Re-check a folder on the drive against its manifest (bit-rot check for the offline copy). |
| `nur nas archive-adopt <share/folder> [--bwlimit 60M]` | For a folder moved some other way (e.g. UGOS File Manager): fix the root-owned `000` directories UGOS leaves, then write its manifest. Refuses if the source still exists or UGOS temp files are present. |
| `nur nas archive-status` | Running and finished jobs, the last journal lines of running ones, drive free space, and the end of `ARCHIVE.log`. |

### Other cluster helpers

| Command | What it does |
|---|---|
| `nur kuma-cli config` | Write `~/.config/kuma/config.toml` (mode 600) from uptime-kuma's ingress host and AutoKuma's credentials secret, so `kuma` works without flags. Re-run after rotating the password. |
| `nur install cert-manager` | Apply the pinned cert-manager release manifest. |
| `nur watch-pods` / `nur watch-namespaces` | `watch kubectl get ...` across all namespaces. |
| `nur k3s wipe` | `sudo rm -rf` this node's `/etc/rancher/k3s` and `/var/lib/rancher/k3s`. **Destroys local k3s state.** |

## Dashboard UI and inbox app

`resources/apps/dashboard-ui/` holds shared React components with a Storybook, deployed by `nur switch k8s` as the `storybook` app. The `ui` tasks install `node_modules` on first use.

| Command | What it does |
|---|---|
| `nur ui dev` | Storybook with live reload on http://localhost:6006, for changes that aren't deployed yet. |
| `nur ui test` | Typecheck plus component tests. |
| `nur ui build` | Nix build of the static Storybook, exactly as the cluster serves it; prints the store path. |
| `nur ui hash` | Recompute `npmDepsHash` in `modules/kubernetes/pkgs/dashboard-ui.nix` after `package-lock.json` changes. |

The inbox review app is a Windmill full-code app at `u/duck/inbox_review`, built from dashboard-ui. It sits outside the `f/` tree that the windmill-sync hook manages, so `nur switch k8s` doesn't deploy it.

| Command | What it does |
|---|---|
| `nur inbox dev [--no-open]` | Local dev server on http://localhost:4567 (opens a browser with `xdg-open` unless `--no-open`). Backend calls run on the real Windmill. |
| `nur inbox push` | Bundle shared `f/lib` modules into the backend and push the app to Windmill. |

Both reinstall the app's copy of dashboard-ui first, so component changes are picked up.

## Host maintenance

| Command | What it does |
|---|---|
| `nur claude drift [--host h]` | Claude Code allow rules in `~/.claude/settings.json` that the shared list in `features/claude.nix` doesn't manage: candidates to promote, or one-offs to delete. Secret-looking assignments are masked. |
| `nur claude prune [--host h] [--apply]` | List one-off allow rules (exact Bash commands, `/nix/store` hashes, `/tmp` or scratchpad paths). `--apply` removes them after backing up `settings.json`. Shared rules are never removed. |
| `nur vscode drift [--host h]` | VS Code settings that don't match Nix: `unmanaged` keys exist only in `settings.json`; `changed` keys were edited in the UI and get reset on the next switch. |
| `nur show-ports` | `netstat -ltnp` |
| `nur apply-git-hooks` | Point `core.hooksPath` at `.githooks/`. No hooks are committed yet. |
