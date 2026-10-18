# Secrets

Secrets are encrypted with [sops](https://github.com/getsops/sops) using **age** keys; no file has any PGP recipients. They live in `secrets/`, and `.sops.yaml` decides who can decrypt each file.

For the `nur secrets` tasks (edit, keys, check, exec, ...), see [nur_commands.md](nur_commands.md#secrets).

## Files

| File | Used by | Decrypted where |
|---|---|---|
| `secrets/k8s.enc.yaml` | Kubernetes app secrets, plus a few host secrets (e.g. wayle's Windmill token) | On the workstation by `nur k8s switch-charts`, which writes each app's secrets into `SopsSecret` manifests (still encrypted; the cluster decrypts them); on hosts by sops-nix |
| `secrets/attic-token.yaml`, `cachix-token.yaml` | `modules/nixos/nix-attic.nix` (binary cache push tokens) | On hosts, by sops-nix |
| `secrets/k3s-token.yaml` | `features/kubernetes.nix` (`tokenFile`) | On k3s nodes, by sops-nix |
| `secrets/mcp.yaml` | `features/claude.nix` (MCP server tokens) | On hosts, by sops-nix |

`k8s.enc.yaml` follows the `*.enc.*` naming, so `nur secrets decrypt`/`encrypt` can round-trip it through a git-ignored plaintext copy. The other files are encrypted in place, so edit them with `nur secrets edit <file>`.

## Who can decrypt

`.sops.yaml` has two creation rules:

- `secrets/k8s.enc.yaml`: the `k8s_primary` key (the personal key you keep in `~/.config/sops/age/keys.txt`) plus the server host keys.
- Every other `secrets/*.{yaml,json,env,ini}`: the server host keys only, including nixmini's.

The host keys are each machine's SSH host key converted to age. sops-nix decrypts with `/etc/ssh/ssh_host_ed25519_key` by default, so no key file needs distributing to hosts.

## Getting your key

`k8s.enc.yaml` (and so `nur switch k8s`) needs the `k8s_primary` private key at `~/.config/sops/age/keys.txt`. Restore it from KeePass:

```sh
export KEEPASS_DB_PATH="${HOME}/keepass/passwords.kdbx"
export SECRET_PATH="/Kubernetes/Age-key"
mkdir -p ~/.config/sops/age
keepassxc-cli show -s -a Password ${KEEPASS_DB_PATH?} ${SECRET_PATH?} > ~/.config/sops/age/keys.txt
```

Check it works without printing anything:

```sh
sops --decrypt secrets/k8s.enc.yaml > /dev/null && echo "OK"
```

A brand-new key (`age-keygen -o ~/.config/sops/age/keys.txt`) can't decrypt anything until its public key is added as a recipient (below) by someone who already can.

## Adding a recipient

1. Get the age public key:
   - for a host: `ssh-to-age < /etc/ssh/ssh_host_ed25519_key.pub` (run on the host, or against a copy of its public key)
   - for a personal key: the `# public key:` line in its `keys.txt`
2. Add it to the key inventory comment and the relevant `creation_rules` in `.sops.yaml`.
3. Re-encrypt each affected file for the new recipient set: `sops updatekeys secrets/<file>`.

`age` and `ssh-to-age` are both in the devShell.

## Rules for agents

**Never put a secret value into the conversation.** Don't run `sops -d` or `nur secrets decrypt`, and don't read `secrets/k8s.yaml`. Find the path with `nur secrets keys`, check it with `nur secrets check`, and use it via `nur secrets exec`, which replaces any secret value in the command's output with `[REDACTED:VAR]`. Reference the variable inside the command (`sh -c '... "$VAR" ...'`), not on the outer command line. Don't pass `--no-redact`.
