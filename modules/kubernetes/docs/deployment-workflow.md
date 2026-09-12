# Deployment Workflow Details

The main CLAUDE.md covers the basic GitOps flow. This page covers two gotchas that are easy to get backwards under pressure (e.g. mid-incident).

## `nur switch` never deploys by itself

`nur switch` (build → post-process → write `manifests/dev/` → write sops secrets → `activate`) only ever writes local files. It does **not** push to git and does **not** talk to the cluster. ArgoCD's `00-master` app-of-apps syncs from the `master` branch of the git remote — nothing takes effect in the cluster until the resulting `manifests/dev/` changes are `git commit`ed and `git push`ed, and ArgoCD picks them up (either on its normal poll interval, or immediately via a hard refresh — see below).

Concretely: after running `nur switch` and confirming the generated manifests look right, the remaining steps are always `git add`/`git commit`/`git push`, not `kubectl apply -f manifests/dev/...`. Applying manifests directly with `kubectl` is a dead end — the next ArgoCD sync (`selfHeal: true` is on) reverts it.

## Scaling/pausing an app: change the Nix source, don't `kubectl scale`

Because `00-master`'s child Applications all inherit `selfHeal: true`, ArgoCD actively reverts drift from the git-defined state — including a manual `kubectl scale deploy --replicas=0` or an attempt to patch `spec.syncPolicy.automated` off on the child `Application` CR directly. Both get reverted within ~10-15 seconds, which is exactly the amount of time you don't have when you're trying to stop a destructive retry loop (e.g. a CSI mount-retry loop actively overwriting a Longhorn volume — see [troubleshooting.md](troubleshooting.md)).

The correct sequence, which actually sticks:

1. Change the desired state in the Nix source — typically `replicas = 0;` under `services.<name>` in `env/dev/<name>.nix`.
2. `nur switch` (or at least `nur switch --fallback` if the local binary cache is being flaky).
3. `git commit` + `git push`.
4. Optionally force immediate reconciliation instead of waiting for ArgoCD's poll interval:
   ```sh
   nur argocd refresh <name>   # or with no name, refreshes every Application
   ```
   (`nur argocd apply-master` does *not* do this -- it only bootstraps `00-master` onto a cluster that doesn't have it yet; re-running it against an already-registered app is a no-op since the object's content hasn't changed.)

This is the same sequence to scale back up afterward. It feels slower than reaching for `kubectl scale` directly, but the direct route simply doesn't work here and wastes more time fighting ArgoCD than it saves.

## `nur build`/`nur switch` failing on Attic (the self-hosted binary cache)

If a build fails with HTTP/2 framing errors or "no substituter that can build it" against `attic.home.kronkltd.net`, that's usually the self-hosted Attic cache being flaky, not a real problem with the derivation. Retry with `nur build --fallback` / `nur switch --fallback` to force a local build instead of insisting on the substituter. See [nix-csi-and-binary-cache.md](nix-csi-and-binary-cache.md) for how that cache is set up and its own failure modes.
