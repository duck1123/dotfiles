# Bootstrapping a new cluster

One-time steps to get ArgoCD running on a fresh k3s cluster and tracking the `argo-manifests` repo. Once this is done, deploying is just `nur switch k8s` (see [deployment-workflow.md](deployment-workflow.md)).

Prerequisites on the workstation: a working kubeconfig for the new cluster, the `k8s_primary` age key ([docs/secrets.md](../../../docs/secrets.md#getting-your-key)), and the manifests checkout (`nur k8s manifests init`).

## 1. Install ArgoCD

The k3s server installs it. Set `kubernetes.argocd.enable = true` on the cluster's server host (it's on `nasnix` for the dev cluster) and switch that host. The kubernetes feature renders `kubernetes/infra-manifests/argocd/` (the pinned `install.yaml` plus the ingress) with kustomize, adds the `argocd` Namespace, and links it into k3s's auto-deploy manifests directory. k3s applies it on startup and whenever the file changes. Enable it on only one server per cluster: k3s doesn't sync AddOns between servers. Turning the option off leaves ArgoCD installed.

To upgrade:

```sh
nur argocd update-manifest v3.5.7    # pin a release tag; read the upstream upgrade guide for each minor you cross
git add -A kubernetes/infra-manifests/argocd && git commit
nur switch os --host nasnix          # k3s re-applies it, no k3s restart
```

`nur argocd install` still works without the Nix feature (for example on a cluster whose server isn't one of these hosts). It applies the same directory with `kubectl apply --server-side` and waits for `argocd-server`, and is safe to re-run.

After a major upgrade, run `nur argocd sync` once so every Application gets the new tracking metadata (ArgoCD 3.0 switched from label to annotation tracking).

## 2. Give ArgoCD access to argo-manifests

ArgoCD needs an SSH deploy key to pull from the private manifests repo. That credential has to be applied directly, since ArgoCD can't sync it from a repo it can't read yet.

If a deploy key is already stored in secrets (`argocd.sshDeployKey` in `secrets/k8s.enc.yaml`):

```sh
nur k8s bootstrap-argocd-repo
```

To create a new one:

```sh
# 1. Generate the key pair
nur k8s generate-deploy-key

# 2. Add the printed PUBLIC key to GitHub:
#    argo-manifests → Settings → Deploy keys → Add deploy key (read-only)

# 3. Store the printed PRIVATE key in secrets under argocd.sshDeployKey:
nur secrets edit
#    argocd:
#      sshDeployKey: |
#        -----BEGIN OPENSSH PRIVATE KEY-----
#        ...
#        -----END OPENSSH PRIVATE KEY-----

# 4. Apply the credential to the cluster
nur k8s bootstrap-argocd-repo
```

## 3. Push manifests and register the app-of-apps

```sh
nur switch k8s
nur argocd apply-master
```

`apply-master` applies `kubernetes/infra-manifests/00-master.yaml`: the `00-master` Application, which tracks `dev/apps` in `argo-manifests`. That directory holds one Application per app, so from here ArgoCD syncs everything on its own (automated sync, prune and selfHeal).

## 4. Log in to the UI

Until the ingress is up, port-forward the UI and open https://localhost:8080/:

```sh
nur forward argocd
```

The initial admin password:

```sh
argocd admin initial-password -n argocd
```
