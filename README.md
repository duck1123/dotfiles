# Duck's dotfiles

This is a single Nix flake that defines an entire personal computing fleet — every NixOS machine, every home-manager profile, and the Kubernetes cluster they all talk to — as one evaluated, reproducible whole. There's no separate "dotfiles" repo plus a separate "infra" repo plus a separate "k8s" repo drifting out of sync with each other; a change here can touch a laptop's window manager config, a server's systemd units, and a cluster app's ingress domain, and all of it is checked, built, and applied through the same handful of commands.

The flake is assembled with [flake-parts](https://github.com/hercules-ci/flake-parts) and [import-tree](https://github.com/vic/import-tree), which auto-imports every module under `./modules/` — adding a new host, feature, or identity is mostly a matter of dropping a file in the right place rather than wiring it into a central list by hand. Everything is driven through one task runner, [nur](https://github.com/nur-taskrunner/nur) (Nushell-based, tasks defined in `scripts/nur.nu`), so `nur switch`, `nur build`, `nur check` and friends work the same way whether the target is a NixOS host, a home-manager profile, or the Kubernetes cluster.

### What's in here

- **NixOS + home-manager** configs for every machine in the fleet — see [Hosts](#hosts) below for the current list, and `CLAUDE.md` for how the module system is organized.
- **Kubernetes fleet management**: application definitions, environment config, secrets, and the automation to build and deploy manifests via ArgoCD/nixidy. See [Kubernetes manifests](#kubernetes-manifests). This used to live in a separate repo (`k3s-fleetops`); it's since been folded in here so the whole fleet — machines and cluster alike — is defined in one place.
- **Secrets management** via [sops-nix](https://github.com/Mic92/sops-nix) with age keys, covering both host-level and cluster secrets.
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
| vavirl-pw0bwnq8 | NixOS-WSL | WSL on Ubuntu, user: drenfer |
| pixel8 | feature/syncthing config only | Android phone; no NixOS or home-manager build target |

---

## Setup

### Clone location

The default and recommended location is `~/dotfiles`. The Nushell config derives all internal paths from `$env.DOTFILES_DIR`, which defaults to `~/dotfiles` but can be overridden by setting it in the environment before launching Nushell.

```sh
git clone git@github.com:duck1123/dotfiles.git ~/dotfiles
```

### Nix (non-NixOS only)

NixOS machines already have Nix. For non-NixOS hosts (steamdeck), use the [Determinate Nix installer](https://github.com/DeterminateSystems/nix-installer), which handles upgrades cleanly and supports WSL out of the box:

```sh
curl --proto '=https' --tlsv1.2 -sSf -L https://install.determinate.systems/nix | sh -s -- install \
  --extra-conf "trusted-users = root $USER"
```

The `trusted-users` flag is required to allow this user to specify substituters (binary caches). Without it, cachix/attic caches are silently ignored and every package builds from source.

Flake support is enabled automatically by the Determinate installer. If you ever need to add local Nix settings, edit `/etc/nix/nix.custom.conf` (Determinate's user-editable overlay — `/etc/nix/nix.conf` is managed by the installer and will be overwritten on upgrade).

### WSL — NixOS image setup

`vavirl-pw0bwnq8` runs as a NixOS-WSL distro, replacing the plain Ubuntu WSL base.

**Step 1** — build the tarball builder:

```sh
nur build --tarball --host vavirl-pw0bwnq8
```

This produces `result/bin/nixos-wsl-tarball-builder` — a self-contained script.

**Step 2** — run the builder as root to produce the image (writes `nixos.wsl` to the current directory):

```sh
sudo result/bin/nixos-wsl-tarball-builder
```

**Step 3** — import it into WSL from inside the Ubuntu WSL shell:

```sh
# cmd.exe is always available in WSL; tr strips the Windows carriage return
WIN_HOME=$(cmd.exe /c "echo %USERPROFILE%" 2>/dev/null | tr -d '\r')
mkdir -p "$(wslpath "$WIN_HOME")/wsl/NixOS"
wsl.exe --unregister NixOS 2>/dev/null || true   # remove any previous failed import
wsl.exe --import NixOS "$WIN_HOME\\wsl\\NixOS" "$(wslpath -w "$(pwd)/nixos.wsl")"
wsl.exe -s NixOS   # set as default distro (optional)
```

After first boot, apply the home-manager config from inside the NixOS WSL shell:

```sh
nur switch home
```

Subsequent NixOS updates deploy via the normal `nur switch os` command.

### Task runner

Tasks are defined in `scripts/nur.nu` and run with [nur](https://github.com/nur-taskrunner/nur), a Nushell-based task runner (similar to `just`, but tasks are plain Nushell `def`s). The `nurfile` at the repo root loads the tasks module via `overlay use scripts/nur.nu`; `nur` discovers it automatically as long as your CWD is the repo root (or a subdirectory of it).

On hosts with the `nushell` home-manager feature enabled, `nur` is already on `PATH`. On a fresh machine, before home-manager has been applied, get a Nushell session with the tasks preloaded instead:

```sh
nix run .#pnu
```

then run `nur <task>` inside that shell.

### direnv

`.envrc` is gitignored — copy the example and edit it before allowing:

```sh
cp .envrc.example .envrc
# Review and fill in any secrets (e.g. TAILSCALE_API_KEY)
direnv allow
```

`use flake` in the file loads the repo's devShell, which provides `nur`, `nh`, `sops`, `age`, `kubectl`, and the other tools listed in `modules/flake/devShells.nix`. Entering the directory activates the environment; leaving it deactivates it.

If direnv is not yet installed, the `direnv` home-manager feature handles that on managed hosts. On a fresh machine before home-manager has run, use `nix run .#pnu` to get a shell with the tools available instead.

### Age key (sops-nix)

All secrets are encrypted with [sops](https://github.com/getsops/sops) using age keys. You need the private key available for decryption.

#### Restore existing key from KeePass

```sh
export KEEPASS_DB_PATH="${HOME}/keepass/passwords.kdbx"
export SECRET_PATH="/Kubernetes/Age-key"
mkdir -p ~/.config/sops/age
keepassxc-cli show -s -a Password ${KEEPASS_DB_PATH?} ${SECRET_PATH?} > ~/.config/sops/age/keys.txt
```

#### Or generate a new key

```sh
mkdir -p ~/.config/sops/age
age-keygen -o ~/.config/sops/age/keys.txt
```

---

## Commands

### List age secret keys

```sh
nur secrets list-keys
```

### Update flake inputs

```sh
nix flake update
```

### Format Nix files

```sh
nur format
```

### Lint Nix files

```sh
nur lint
```

---

## NixOS / home-manager

### Apply local configuration

```sh
nur switch                       # apply both home-manager and NixOS
nur switch home                  # home-manager only
nur switch os                    # NixOS only
nur switch k8s                   # build and push k8s manifests only (no --host)
nur switch os --boot             # set NixOS as boot default instead of activating (safe for slow activations)
```

### Remote deployment

All builds happen locally (with `nom` for better progress display), then the result is copied and activated on the remote host.

#### Build only (no activation)

```sh
nur build --host edgenix
nur build --host nasnix
```

#### Show package changes (diff)

```sh
nur diff-os --host edgenix
nur diff-os --host nasnix
```

#### Dry run (preview without applying)

```sh
nur dry-run-os --host edgenix
nur dry-run-os --host nasnix
```

#### Switch (build and activate)

```sh
nur switch --host edgenix                     # both home-manager and NixOS
nur switch --host nasnix

nur switch --host edgenix os                  # NixOS only
nur switch --host edgenix home                # home-manager only
```

**Prerequisites:**
- SSH key-based auth configured for the target host (e.g. `edgenix`, `nasnix`)
- Remote user has sudo access (tasks prompt for the sudo password when switching NixOS)

---

## Kubernetes manifests

The cluster's application definitions, generators, and shared library code live under `modules/kubernetes/_vendor/`; the environment-specific config (which apps are enabled, their domains, storage, secrets wiring) lives in `modules/kubernetes/_env/dev/`. Building that config produces Kubernetes manifests via [nixidy](https://github.com/arnarg/nixidy), which get pushed to a private [argo-manifests](https://github.com/duck1123/argo-manifests) repo; ArgoCD on the cluster syncs from there. See `modules/kubernetes/docs/` for the deployment workflow in more depth, the pinned-volumes convention, and a troubleshooting playbook.

```
dotfiles/
  modules/kubernetes/
    _vendor/              ← application definitions, generators, shared library code
    _env/dev/              ← this environment's config (services, domains, storage)
  secrets/k8s.enc.yaml     ← encrypted cluster secrets (sops/age)
  kubernetes/
    infra-manifests/       ← ArgoCD bootstrap manifests (install, 00-master app-of-apps)
    manifests/              ← checkout of argo-manifests (gitignored here)
```

### Ongoing workflow

```sh
nur k8s deploy          # build manifests + push to argo-manifests (most common)

# or step by step:
nur k8s switch-charts   # build nixidy manifests → write to kubernetes/manifests/
nur k8s push            # commit + push kubernetes/manifests/ to argo-manifests

nur k8s edit-secrets    # edit cluster secrets in-place with sops
```

Cluster operations beyond deploying — restarting an app, ArgoCD sync/refresh, port-forwarding, database backup/restore — are also `nur` tasks; see `CLAUDE.md`'s Key Commands for a starting list, or `nur --help` for the full set.

### First-time setup on a new machine

#### 1. Clone the private manifests repo

ArgoCD reads manifests from [argo-manifests](https://github.com/duck1123/argo-manifests). Clone it inside this repo (it is gitignored here):

```sh
git clone git@github.com:duck1123/argo-manifests.git kubernetes/manifests
```

#### 2. Verify your age key can decrypt cluster secrets

```sh
sops --decrypt secrets/k8s.enc.yaml > /dev/null && echo "OK"
```

#### 3. Build and push manifests

```sh
nur k8s deploy
```

### Bootstrap a new cluster

Run these steps when setting up ArgoCD on a fresh cluster for the first time.

#### Install ArgoCD

```sh
nur install argocd
```

#### Configure the argo-manifests deploy key

ArgoCD needs an SSH deploy key to pull from the private manifests repo. This credential must be applied directly (ArgoCD cannot sync it from the repo it does not yet have access to).

**If you already have a deploy key stored in secrets:**

```sh
nur k8s bootstrap-argocd-repo
```

**If you need to create a new deploy key:**

```sh
# 1. Generate the key pair
nur k8s generate-deploy-key

# 2. Add the printed PUBLIC key to GitHub:
#    argo-manifests → Settings → Deploy keys → Add deploy key (read-only)

# 3. Store the printed PRIVATE key in secrets:
nur k8s edit-secrets
#    Add under key:
#    argocd:
#      sshDeployKey: |
#        -----BEGIN OPENSSH PRIVATE KEY-----
#        ...
#        -----END OPENSSH PRIVATE KEY-----

# 4. Apply the credential to the cluster
nur k8s bootstrap-argocd-repo
```

#### Push manifests and apply the master application

```sh
nur k8s deploy

# Apply the generated ArgoCD Application manifests so ArgoCD starts tracking them
kubectl apply -f kubernetes/manifests/dev/apps/
```

Each `Application-*.yaml` is self-managed (automated sync + prune), so once applied ArgoCD will keep syncing all applications from the manifests repo on its own.

#### Get the initial ArgoCD password

```sh
argocd admin initial-password -n argocd
```

#### Forward the ArgoCD UI (before ingress is ready)

```sh
kubectl port-forward svc/argocd-server -n argocd 8080:443
```

Then open https://localhost:8080/

### Managing secrets

All cluster secrets live in `secrets/k8s.enc.yaml` (encrypted with sops/age).

Edit in-place (no plaintext file written to disk):

```sh
nur k8s edit-secrets
# or directly: sops secrets/k8s.enc.yaml
```

Decrypt → edit → re-encrypt:

```sh
nur k8s decrypt           # → secrets/k8s.yaml  (DO NOT commit)
# edit secrets/k8s.yaml
nur k8s encrypt           # → secrets/k8s.enc.yaml
rm secrets/k8s.yaml
```

---

## Validation

```sh
nur check              # nix flake check
nur lint               # lint .nix files with statix
nur build --all        # build all configurations
```

### Reboot

```sh
sudo reboot
```
