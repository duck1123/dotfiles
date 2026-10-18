# Deployment Workflow Details

[docs/nur_commands.md](../../../docs/nur_commands.md#kubernetes-manifests) lists the deploy and cluster tasks. This page covers the gotchas that are easy to get backwards under pressure (e.g. mid-incident).

## What actually deploys

ArgoCD's `00-master` app-of-apps syncs from the private `argo-manifests` repo (`git@github.com:duck1123/argo-manifests.git`), checked out at `kubernetes/manifests/` (`nur k8s manifests init`). Nothing in this repo talks to the cluster directly; a change takes effect only once rendered manifests are pushed there.

- `nur k8s switch-charts [--fallback]` builds the nixidy env and writes `kubernetes/manifests/dev/` (plus sops secrets, post-processing, and pushing pinned store paths to Attic). Local files only, nothing deployed.
- `nur k8s push` commits and pushes whatever changed in `kubernetes/manifests/` — **this is the deploy step**.
- `nur switch k8s` (= `nur k8s deploy`) does `manifests sync` + `switch-charts` + `push` in one go, so it **does deploy**. Agents: treat it as outward-facing and confirm before running it; use `nur k8s switch-charts` when you only want to inspect the rendered diff (`git -C kubernetes/manifests diff`).

The Nix source change itself still needs committing to this (dotfiles) repo separately.

Applying manifests directly with `kubectl apply` is a dead end — the next ArgoCD sync (`selfHeal: true` is on) reverts it.

## Scaling/pausing an app: change the Nix source, don't `kubectl scale`

Because `00-master`'s child Applications all inherit `selfHeal: true`, ArgoCD actively reverts drift from the git-defined state — including a manual `kubectl scale deploy --replicas=0` or an attempt to patch `spec.syncPolicy.automated` off on the child `Application` CR directly. How fast it reverts is unpredictable (observed anywhere from ~10s to ~2 minutes), so **never** use a manual scale to open a window for a destructive or stateful step (deleting a PVC, pinning a volume, migrating a database, stopping a CSI mount-retry loop that's overwriting a Longhorn volume — see [troubleshooting.md](troubleshooting.md)). `kubectl scale` is only fine for read-only poking around.

The correct sequence, which actually sticks:

1. Change the desired state in the Nix source — typically `replicas = 0;` under `services.<name>` in `clusters/dev/apps/<name>.nix` (add a `replicas` option to the app template if it lacks one).
2. `nur switch k8s` (or `nur k8s switch-charts --fallback` + `nur k8s push` if the local binary cache is being flaky).
3. Force immediate reconciliation instead of waiting for ArgoCD's poll interval:
   ```sh
   nur argocd refresh <name>   # or with no name, refreshes every Application
   ```
   (`nur argocd apply-master` does *not* do this -- it only bootstraps `00-master` onto a cluster that doesn't have it yet.)
4. Wait for the pods to actually terminate before doing the stateful step.

This is the same sequence to scale back up afterward. It feels slower than reaching for `kubectl scale` directly, but the direct route simply doesn't work here and wastes more time fighting ArgoCD than it saves.

## Refresh vs. sync vs. terminate

- `nur argocd refresh <name>` only re-diffs against git. It does **not** start a new operation when the last one ended `Failed` with its retry budget exhausted (e.g. after a pinned-volume replace kept failing) — the app just stays `OutOfSync`/`Failed`. Use `nur argocd sync <name>` for that.
- `PostSync`/`Sync` hooks (e.g. `postgresql-init-databases`, which creates per-app DB users) only run on a real sync, never on a refresh.
- A hook Job stuck in progress ("waiting for completion of hook", or `FailedPrecondition: another operation is already in progress`) blocks every later sync. Neither a refresh nor deleting the Job clears it; terminate the operation first (from the dev shell): `argocd app terminate-op <name> --core`. Check `status.operationState.phase`/`message` on the Application to confirm this is what's happening.
- `--core` mode reads ArgoCD's config from the kube context's *current namespace*, and fails with `configmap "argocd-cm" not found` if that isn't `argocd`. `nur argocd sync` handles this itself (it runs `argocd` against a temporary kubeconfig set to the `argocd` namespace), but a hand-run `argocd ... --core` like the `terminate-op` above doesn't: switch first with `kubectl config set-context --current --namespace=argocd`.

## Secret value changes don't restart pods

Changing a sops secret's *value* updates the k8s Secret, but env vars from `secretKeyRef` are read at container start, so running pods keep the old value. Restart the app afterward: `nur apps restart <name>`.

## `nur build`/`nur k8s switch-charts` failing on Attic (the self-hosted binary cache)

If a build fails with HTTP/2 framing errors, `NoSuchKey`, or "no substituter that can build it" against `attic.home.kronkltd.net`, that's usually the self-hosted Attic cache, not a real problem with the derivation. Retry with `--fallback` to force a local build. If it keeps happening, check attic's logs — repeated `NoSuchKey` means its Postgres metadata has drifted from Garage's contents (see [nix-csi-and-binary-cache.md](nix-csi-and-binary-cache.md)).
