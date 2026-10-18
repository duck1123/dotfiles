# Bootstrapping a new cluster

One-time steps to get ArgoCD running on a fresh k3s cluster and tracking the `argo-manifests` repo. Once this is done, deploying is just `nur switch k8s` (see [deployment-workflow.md](deployment-workflow.md)).

Prerequisites on the workstation: a working kubeconfig for the new cluster, the `k8s_primary` age key ([docs/secrets.md](../../../docs/secrets.md#getting-your-key)), and the manifests checkout (`nur k8s manifests init`).

## 1. Install ArgoCD

```sh
nur argocd install
```

This applies the pinned `kubernetes/infra-manifests/argocd/install.yaml` (downloading it first if it's missing) and waits for `argocd-server`. It's safe to re-run, and doubles as the upgrade path after `nur argocd update-manifest`.

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
