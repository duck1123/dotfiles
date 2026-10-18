# Setting up a machine

Getting from a bare machine to one that runs `nur switch` from this repo. To add a machine that doesn't have a host file yet, first create one (see [architecture.md](architecture.md#adding-a-host)).

## 1. Clone

The default and recommended location is `~/dotfiles`. The Nushell config derives its internal paths from `$env.DOTFILES_DIR`, which defaults to `~/dotfiles`; set it in the environment before launching Nushell to use another location.

```sh
git clone git@github.com:duck1123/dotfiles.git ~/dotfiles
```

## 2. Nix (non-NixOS hosts only)

NixOS machines already have Nix. For home-manager-only hosts (steamdeck), use the [Determinate Nix installer](https://github.com/DeterminateSystems/nix-installer), which handles upgrades cleanly:

```sh
curl --proto '=https' --tlsv1.2 -sSf -L https://install.determinate.systems/nix | sh -s -- install \
  --extra-conf "trusted-users = root $USER"
```

`trusted-users` lets this user set substituters (binary caches). Without it, the cachix/Attic caches are silently ignored and every package builds from source.

The Determinate installer enables flakes itself. Put any local Nix settings in `/etc/nix/nix.custom.conf`: `/etc/nix/nix.conf` is managed by the installer and gets overwritten on upgrade.

## 3. Tools: `nur` and the devShell

Everything runs through `nur` (see [nur_commands.md](nur_commands.md)). Until home-manager has been applied, get it one of two ways:

- `nix run .#pnu`: a Nushell session with the tasks loaded; run `nur <task>` inside it.
- direnv: `.envrc` is git-ignored, so copy the example, fill it in, and allow it. Its `use flake` loads the devShell (`modules/flake/devShells.nix`), which provides `nur`, `nh`, `sops`, `age`, `ssh-to-age`, `kubectl` and more.

  ```sh
  cp .envrc.example .envrc
  # review and fill in any secrets (e.g. TAILSCALE_API_KEY)
  direnv allow
  ```

Once home-manager is applied, the `nushell` feature puts `nur` on `PATH` and the `common` feature enables direnv (with nix-direnv).

## 4. Secrets key (only where you'll deploy the cluster)

Hosts decrypt their own sops-nix secrets with their SSH host key, so a machine only needs a personal age key to deploy Kubernetes manifests. See [secrets.md](secrets.md#getting-your-key) for restoring it. A new host does need its host key added as a recipient before its secrets will decrypt (see [secrets.md](secrets.md#adding-a-recipient)).

## 5. First switch

```sh
nur switch            # home-manager + NixOS
nur switch home       # home-manager only (steamdeck)
nur bootstrap-os      # fresh NixOS install where nix-command/flakes aren't enabled yet
```

Once a host is reachable over SSH, you can deploy to it from another machine instead: `nur switch --host <name>`.

## 6. Kubernetes manifests checkout (deploying machines only)

To deploy the cluster from this machine, clone the private manifests repo into `kubernetes/manifests/` (git-ignored here), then deploy:

```sh
nur k8s manifests init
nur switch k8s
```

To set up a new cluster from scratch, see [modules/kubernetes/docs/bootstrap.md](../modules/kubernetes/docs/bootstrap.md).

## WSL: building the NixOS image

`vavirl-pw0bwnq8` runs as a NixOS-WSL distro, replacing the plain Ubuntu WSL base.

1. Build the tarball builder, which produces `result/bin/nixos-wsl-tarball-builder` (a self-contained script):

   ```sh
   nur build --tarball --host vavirl-pw0bwnq8
   ```

2. Run it as root to produce the image (writes `nixos.wsl` to the current directory):

   ```sh
   sudo result/bin/nixos-wsl-tarball-builder
   ```

3. Import it into WSL from inside the Ubuntu WSL shell:

   ```sh
   # cmd.exe is always available in WSL; tr strips the Windows carriage return
   WIN_HOME=$(cmd.exe /c "echo %USERPROFILE%" 2>/dev/null | tr -d '\r')
   mkdir -p "$(wslpath "$WIN_HOME")/wsl/NixOS"
   wsl.exe --unregister NixOS 2>/dev/null || true   # remove any previous failed import
   wsl.exe --import NixOS "$WIN_HOME\\wsl\\NixOS" "$(wslpath -w "$(pwd)/nixos.wsl")"
   wsl.exe -s NixOS   # set as default distro (optional)
   ```

4. After first boot, apply home-manager from inside the NixOS WSL shell with `nur switch home`. Later NixOS updates use the normal `nur switch os`.
