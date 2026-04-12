#!/usr/bin/env bash
# Apply the ArgoCD repository credential for git@github.com:duck1123/argo-manifests.git
# to the cluster using kubectl (bypassing ArgoCD — this is the bootstrap step).
#
# This must be run ONCE before ArgoCD can sync from the private manifests repo.
# After this the credential is live in the cluster and ArgoCD manages everything else.
#
# Prerequisites:
#   - kubectl configured and pointing at your cluster
#   - argocd namespace exists (run bb install-argocd first if needed)
#   - secrets/k8s.enc.yaml contains argocd.sshDeployKey (the private half of your deploy key)
#     See: bb k8s-generate-deploy-key to create the key pair
#
# Usage:
#   ./scripts/k8s-bootstrap-argocd-repo.sh
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
DOTFILES_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
REPO_URL="git@github.com:duck1123/argo-manifests.git"

# ---------------------------------------------------------------------------
# 1. Decrypt secrets and extract the SSH deploy key
# ---------------------------------------------------------------------------
TMP="$(mktemp)"
trap 'rm -f "$TMP"' EXIT

sops --decrypt "$DOTFILES_ROOT/secrets/k8s.enc.yaml" > "$TMP"

SSH_PRIVATE_KEY="$(yq '.argocd.sshDeployKey' "$TMP")"

if [[ -z "$SSH_PRIVATE_KEY" || "$SSH_PRIVATE_KEY" == "null" ]]; then
  echo "ERROR: argocd.sshDeployKey not found in secrets/k8s.enc.yaml" >&2
  echo "" >&2
  echo "Generate a deploy key first:" >&2
  echo "  bb k8s-generate-deploy-key" >&2
  echo "" >&2
  echo "Then add the private key to secrets/k8s.enc.yaml:" >&2
  echo "  bb k8s-edit-secrets" >&2
  echo "" >&2
  echo "Add under key: argocd:" >&2
  echo "    sshDeployKey: |" >&2
  echo "      -----BEGIN OPENSSH PRIVATE KEY-----" >&2
  echo "      ..." >&2
  echo "      -----END OPENSSH PRIVATE KEY-----" >&2
  exit 1
fi

# ---------------------------------------------------------------------------
# 2. Apply the ArgoCD repository credential secret
# ---------------------------------------------------------------------------
echo "Applying ArgoCD repository credential for $REPO_URL ..."

kubectl apply -f - <<EOF
apiVersion: v1
kind: Secret
metadata:
  name: argo-manifests-repo-creds
  namespace: argocd
  labels:
    argocd.argoproj.io/secret-type: repository
stringData:
  type: git
  url: $REPO_URL
  sshPrivateKey: |
$(echo "$SSH_PRIVATE_KEY" | sed 's/^/    /')
EOF

echo ""
echo "Done. ArgoCD can now read from $REPO_URL"
echo "Verify with: kubectl get secret -n argocd argo-manifests-repo-creds"
