---
runme:
  id: 01J9FJ90VF5CQ5QE6TE6JQ4GR0
  version: v3
---

# Duck's dotfiles

Nix flake-based system configuration managing multiple NixOS hosts and home-manager configurations. Uses [flake-parts](https://github.com/hercules-ci/flake-parts) + [import-tree](https://github.com/vic/import-tree) to auto-import all modules from `./modules/`. Also manages Kubernetes manifests via [nixidy](https://github.com/arnarg/nixidy), pushing generated YAML to a private [argo-manifests](https://github.com/duck1123/argo-manifests) repo for ArgoCD to sync.

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

```sh {"name":"install-nix"}
curl --proto '=https' --tlsv1.2 -sSf -L https://install.determinate.systems/nix | sh -s -- install \
  --extra-conf "trusted-users = root $USER"
```

The `trusted-users` flag is required to allow this user to specify substituters (binary caches). Without it, cachix/attic caches are silently ignored and every package builds from source.

Flake support is enabled automatically by the Determinate installer. If you ever need to add local Nix settings, edit `/etc/nix/nix.custom.conf` (Determinate's user-editable overlay — `/etc/nix/nix.conf` is managed by the installer and will be overwritten on upgrade).

### WSL — NixOS image setup

`vavirl-pw0bwnq8` runs as a NixOS-WSL distro, replacing the plain Ubuntu WSL base. Build the tarball from this flake and import it:

```sh {"name":"build-wsl-image"}
nur build --tarball --host vavirl-pw0bwnq8
```

Then import it from inside the Ubuntu WSL shell (not PowerShell — bash glob expansion and `wslpath` make path handling much cleaner):

```sh {"name":"import-wsl-image"}
# Resolve the symlink and convert to a Windows path that wsl.exe can read
tarball=$(wslpath -w "$(readlink -f result/tarball/*.tar.gz)")
# $USERPROFILE is the Windows home dir (e.g. C:\Users\drenfer), available in WSL
wsl.exe --import NixOS "$USERPROFILE\\wsl\\NixOS" "$tarball"
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

### Age key (sops-nix)

All secrets are encrypted with [sops](https://github.com/getsops/sops) using age keys. You need the private key available for decryption.

#### Restore existing key from KeePass

```sh {"name":"restore-age-key"}
export KEEPASS_DB_PATH="${HOME}/keepass/passwords.kdbx"
export SECRET_PATH="/Kubernetes/Age-key"
mkdir -p ~/.config/sops/age
keepassxc-cli show -s -a Password ${KEEPASS_DB_PATH?} ${SECRET_PATH?} > ~/.config/sops/age/keys.txt
```

#### Or generate a new key

```sh {"name":"create-age-key"}
mkdir -p ~/.config/sops/age
age-keygen -o ~/.config/sops/age/keys.txt
```

---

## Commands

### List age secret keys

```sh {"name":"list-secret-keys"}
nur secrets list-keys
```

### Update flake inputs

```sh {"name":"update-flakes"}
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

```sh {"name":"build-remote"}
nur build --host edgenix
nur build --host nasnix
```

#### Show package changes (diff)

```sh {"name":"diff-remote"}
nur diff-os --host edgenix
nur diff-os --host nasnix
```

#### Dry run (preview without applying)

```sh {"name":"dry-run-remote"}
nur dry-run-os --host edgenix
nur dry-run-os --host nasnix
```

#### Switch (build and activate)

```sh {"name":"switch-remote"}
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

Kubernetes applications are defined in [k3s-fleetops](https://github.com/duck1123/k3s-fleetops) (app definitions + library). This repo holds the environment configuration, secrets, and automation for building and pushing generated YAML manifests to the private [argo-manifests](https://github.com/duck1123/argo-manifests) repo. ArgoCD on the cluster syncs from there.

```
k3s-fleetops/          ← application definitions, library (read-only dependency)
dotfiles/
  modules/kubernetes/
    _env/dev.nix        ← cluster environment config (services, domains, storage)
  secrets/k8s.enc.yaml ← encrypted cluster secrets (sops/age)
  kubernetes/manifests/← checkout of argo-manifests (gitignored here)
```

### Ongoing workflow

```sh
nur k8s deploy          # build manifests + push to argo-manifests (most common)

# or step by step:
nur k8s switch-charts   # build nixidy manifests → write to kubernetes/manifests/
nur k8s push            # commit + push kubernetes/manifests/ to argo-manifests

nur k8s edit-secrets    # edit cluster secrets in-place with sops
```

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

```sh {"id":"01J9HAPD89ZH24ER7CPMKQ1FJW","name":"get-initial-password"}
argocd admin initial-password -n argocd
```

#### Forward the ArgoCD UI (before ingress is ready)

```sh {"background":"true","id":"01J9HAPD89ZH24ER7CPRARMG51","interactive":"false","name":"forward-argocd-ports"}
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

```sh {"name": "reboot"}
sudo reboot
```
