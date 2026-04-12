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
| edgenix | NixOS x86_64 | Server |
| inspernix | NixOS x86_64 | Laptop |
| nasnix | NixOS x86_64 | Virtualized server on NAS, runs k3s |
| powerspecnix | NixOS x86_64 | Primary gaming PC |
| vidcentre | NixOS x86_64 | |
| steamdeck | home-manager only | user: deck |
| vavirl-pw0bwnq8 | home-manager only | WSL, user: drenfer |

---

## Setup

### Babashka

The primary task runner. On NixOS it is available in the dev shell (`nix develop`). On other platforms install it directly:

```sh
bash < <(curl -s https://raw.githubusercontent.com/babashka/babashka/master/install)
```

### Nix (non-NixOS only)

NixOS machines already have Nix. For home-manager-only hosts (steamdeck, WSL):

```sh {"name":"install-nix"}
sh <(curl -L https://nixos.org/nix/install) --daemon
```

Flake support is enabled automatically via the `nixConfig` block in `flake.nix`. If you are on a plain Nix install that does not read `nixConfig`, enable it manually:

```sh {"name":"set-nix-conf"}
mkdir -p ~/.config/nix
echo "experimental-features = nix-command flakes" >> ~/.config/nix/nix.conf
```

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

### List tasks

```sh {"id":"01J9FJBMKK4X3G3KXBJSKAYT27","name":"tasks"}
bb tasks
```

### List age secret keys

```sh {"id":"01J9FJC4985HK894NR72G3730R","interpreter":"","name":"list-secret-keys"}
bb list-secret-keys
```

### Update flake inputs

```sh {"id":"01JBQ87VEQZV4YCB22HYQEHGFS","name":"update-flakes"}
nix flake update
```

### Format Nix files

```sh
bb format
```

---

## NixOS / home-manager

### Apply local configuration

```sh
bb switch          # apply both home-manager and NixOS
bb switch-home     # home-manager only
bb switch-os       # NixOS only
bb boot-os         # build NixOS and set as boot default (safe for slow activations)
```

Using `nh` directly:

```sh {"name":"switch-home"}
nh home switch . -b backup
```

```sh {"name":"switch-os"}
nh os switch .
```

### Remote deployment

All builds happen locally (with `nom` for better progress display), then the result is copied and activated on the remote host.

#### Build only (no activation)

```sh {"name":"build-remote"}
bb build-remote-os-edgenix
bb build-remote-os-nasnix

bb build-remote-home-edgenix
bb build-remote-home-nasnix
```

#### Show package changes (diff)

```sh {"name":"diff-remote"}
bb diff-remote-os-edgenix
bb diff-remote-os-nasnix
```

#### Dry run (preview without applying)

```sh {"name":"dry-run-remote"}
bb dry-run-remote-os-edgenix
bb dry-run-remote-os-nasnix
```

#### Switch (build and activate)

```sh {"name":"switch-remote"}
bb switch-remote               # both hosts, NixOS + home-manager
bb switch-remote-edgenix       # edgenix only
bb switch-remote-nasnix        # nasnix only

bb switch-remote-os-edgenix    # NixOS only
bb switch-remote-home-edgenix  # home-manager only
```

**Prerequisites:**
- SSH key-based auth configured for `edgenix` and `nasnix`
- Remote user has sudo access (tasks prompt for the sudo password when switching NixOS)

---

## Kubernetes manifests

Kubernetes applications are defined in [k3s-fleetops](https://github.com/duck1123/k3s-fleetops) (app definitions + library). This repo holds the environment configuration, secrets, and automation for building and pushing generated YAML manifests to the private [argo-manifests](https://github.com/duck1123/argo-manifests) repo. ArgoCD on the cluster syncs from there.

```
k3s-fleetops/          ← application definitions, library (read-only dependency)
dotfiles/
  modules/kubernetes/
    env/dev.nix        ← cluster environment config (services, domains, storage)
  secrets/k8s.enc.yaml ← encrypted cluster secrets (sops/age)
  kubernetes/manifests/← checkout of argo-manifests (gitignored here)
```

### Ongoing workflow

```sh
bb k8s-deploy          # build manifests + push to argo-manifests (most common)

# or step by step:
bb k8s-switch-charts   # build nixidy manifests → write to kubernetes/manifests/
bb k8s-push            # commit + push kubernetes/manifests/ to argo-manifests

bb k8s-edit-secrets    # edit cluster secrets in-place with sops
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
bb k8s-deploy
```

### Bootstrap a new cluster

Run these steps when setting up ArgoCD on a fresh cluster for the first time.

#### Install ArgoCD

```sh
bb install-argocd
```

#### Configure the argo-manifests deploy key

ArgoCD needs an SSH deploy key to pull from the private manifests repo. This credential must be applied directly (ArgoCD cannot sync it from the repo it does not yet have access to).

**If you already have a deploy key stored in secrets:**

```sh
bb k8s-bootstrap-argocd-repo
```

**If you need to create a new deploy key:**

```sh
# 1. Generate the key pair
bb k8s-generate-deploy-key

# 2. Add the printed PUBLIC key to GitHub:
#    argo-manifests → Settings → Deploy keys → Add deploy key (read-only)

# 3. Store the printed PRIVATE key in secrets:
bb k8s-edit-secrets
#    Add under key:
#    argocd:
#      sshDeployKey: |
#        -----BEGIN OPENSSH PRIVATE KEY-----
#        ...
#        -----END OPENSSH PRIVATE KEY-----

# 4. Apply the credential to the cluster
bb k8s-bootstrap-argocd-repo
```

#### Push manifests and apply the master application

```sh
bb k8s-deploy

# Apply the root ArgoCD Application that points ArgoCD at the manifests repo
bb apply-master-application
```

ArgoCD will now sync all applications from the manifests repo.

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
bb k8s-edit-secrets
# or directly: sops secrets/k8s.enc.yaml
```

Decrypt → edit → re-encrypt:

```sh
bb k8s-decrypt           # → secrets/k8s.yaml  (DO NOT commit)
# edit secrets/k8s.yaml
bb k8s-encrypt           # → secrets/k8s.enc.yaml
rm secrets/k8s.yaml
```

---

## Validation

```sh
bb check      # nix flake check
bb build-all  # build all configurations
```

### Reboot

```sh {"name": "reboot"}
sudo reboot
```
