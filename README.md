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

### Hello World

```sh {"id":"01J9FJ8542CXXDY3FQ8M9CP0GQ","name":"hello"}
echo "Hello World"
```

### List tasks

```sh {"id":"01J9FJBMKK4X3G3KXBJSKAYT27","name":"tasks"}
bb tasks
```

### List Secret Keys

```sh {"id":"01J9FJC4985HK894NR72G3730R","interpreter":"","name":"list-secret-keys"}
bb list-secret-keys
```

### Update Flakes

```sh {"id":"01JBQ87VEQZV4YCB22HYQEHGFS","name":"update-flakes"}
nix flake update
```

### Switch Home

```sh {"id":"01JBQ87VEQZV4YCB22J1ZVN4HG","name":"switch-home-powerspecnix"}
home-manager switch --flake ${HOME?}/dotfiles#duck --impure
```

### Switch OS

```sh {"id":"01JBQ8FGQ0KTGY9B5CRNM3DHRZ","name":"switch-os-powerspecnix"}
sudo nixos-rebuild switch --flake ${HOME?}/dotfiles#nixos
```
