---
runme:
  id: 01J9FJ90VF5CQ5QE6TE6JQ4GR0
  version: v3
---

# Duck's dotfiles

## Setup

### Ubuntu

- https://ubuntu.com/#download

### Babashka

- https://github.com/babashka/babashka

```sh
bash < <(curl -s https://raw.githubusercontent.com/babashka/babashka/master/install)
```

### Nix

#### Install Nix

Recommended install from https://nixos.org/download/

Note: This will request sudo

```sh {"name":"install-nix"}
sh <(curl -L https://nixos.org/nix/install) --daemon
```

#### Enable "Experimental" features

Flakes support requires a feature flag to be set still.

```sh {"name":"set-nix-conf"}
mkdir -p ~/.config/nix
echo "experimental-features = nix-command flakes" > ~/.config/nix/nix.conf
```

### Prelude

- https://github.com/bbatsov/prelude

```sh
curl -L https://git.io/epre | sh
```

## Commands

### List tasks

List all tasks

```sh {"id":"01J9FJBMKK4X3G3KXBJSKAYT27","name":"tasks"}
bb tasks
```

### List Secret Keys

List GPG secret keys

```sh {"id":"01J9FJC4985HK894NR72G3730R","interpreter":"","name":"list-secret-keys"}
bb list-secret-keys
```

### Update Flakes

Update package lock files

```sh {"id":"01JBQ87VEQZV4YCB22HYQEHGFS","name":"update-flakes"}
nix flake update
```

### Switch Home

#### NH

Update user-level configuration

```sh {"name":"switch-home"}
nh home switch ${HOME?}/dotfiles -- --impure --show-trace
```

#### Babashka

```sh
bb switch-home
```

#### Nushell

```nushell
switch home
```

### Switch OS

Update OS configuration

```sh {"name":"switch-os"}
nh os switch ${HOME?}/dotfiles -- --impure --show-trace
```

### Remote Deployment

Deploy to remote servers (edgenix and nasnix). All builds happen locally with `nom` for better progress display, then packages are copied to remote hosts.

#### Build Only (No Activation)

Build configurations locally to see what will be deployed:

```sh {"name":"build-remote"}
# Build NixOS configs (no activation)
bb build-remote-os-edgenix
bb build-remote-os-nasnix

# Build home-manager configs (no activation)
bb build-remote-home-edgenix
bb build-remote-home-nasnix
```

#### Show Package Changes (Diff)

See what packages will change between current and new system (similar to `nh` local diffs):

```sh {"name":"diff-remote"}
# Show package changes for edgenix
bb diff-remote-os-edgenix

# Show package changes for nasnix
bb diff-remote-os-nasnix
```

#### Dry Run (See What Would Change)

Preview changes without applying them:

```sh {"name":"dry-run-remote"}
# See what would change on edgenix
bb dry-run-remote-os-edgenix

# See what would change on nasnix
bb dry-run-remote-os-nasnix
```

#### Switch (Build and Activate)

Build locally and activate on remote:

```sh {"name":"switch-remote"}
# Switch both NixOS and home-manager on both hosts
bb switch-remote

# Switch both NixOS and home-manager on specific host
bb switch-remote-edgenix
bb switch-remote-nasnix

# Switch only NixOS (builds with nom locally, then copies and activates remotely)
bb switch-remote-os-edgenix
bb switch-remote-os-nasnix

# Switch only home-manager (builds with nom locally, copies, then activates remotely)
bb switch-remote-home-edgenix
bb switch-remote-home-nasnix
```

**How it works:**
- **NixOS**: Builds with `nom build` locally, then uses `nixos-rebuild switch --target-host` to copy and activate
- **Home-manager**: Builds activation package with `nom build` locally, copies with `nix copy`, then activates remotely

**Prerequisites:**
- SSH key-based authentication set up for `edgenix` and `nasnix`
- Remote hosts accessible via their hostnames (configure `~/.ssh/config` if needed)
- Remote hosts must have Nix installed and configured to accept SSH-based store access
- Remote user needs sudo access (tasks will prompt for sudo password when switching NixOS configurations)

### Reboot

Restart the computer

```sh {"name": "reboot"}
sudo reboot
```
