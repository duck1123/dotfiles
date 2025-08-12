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

- https://nixos.org/download.html

```sh
sh <(curl -L https://nixos.org/nix/install) --daemon
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

### Reboot

Restart the computer

```sh {"name": "reboot"}
sudo reboot
```
