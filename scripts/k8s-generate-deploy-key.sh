#!/usr/bin/env bash
# Generate an SSH ed25519 key pair for use as a GitHub deploy key on argo-manifests.
#
# After running this:
#   1. Add the PUBLIC key to GitHub → argo-manifests repo → Settings → Deploy keys
#      (read-only access is sufficient)
#   2. Add the PRIVATE key to secrets/k8s.enc.yaml under argocd.sshDeployKey
#      Run: bb k8s-edit-secrets
#      Add:
#        argocd:
#          sshDeployKey: |
#            -----BEGIN OPENSSH PRIVATE KEY-----
#            ...
#            -----END OPENSSH PRIVATE KEY-----
#   3. Run: bb k8s-bootstrap-argocd-repo
#      (after ArgoCD is installed in the cluster)
set -euo pipefail

KEY_FILE="argo-manifests-deploy-key"

if [[ -f "$KEY_FILE" || -f "${KEY_FILE}.pub" ]]; then
  echo "Key files already exist: $KEY_FILE / ${KEY_FILE}.pub"
  echo "Remove them first if you want to regenerate."
  exit 1
fi

ssh-keygen -t ed25519 -f "$KEY_FILE" -N "" -C "argocd-deploy-key@argo-manifests"

echo ""
echo "=== PUBLIC KEY (add to GitHub → argo-manifests → Settings → Deploy keys) ==="
cat "${KEY_FILE}.pub"
echo ""
echo "=== PRIVATE KEY (add to secrets/k8s.enc.yaml under argocd.sshDeployKey) ==="
cat "$KEY_FILE"
echo ""
echo "Delete the plaintext key files after you've stored them:"
echo "  rm $KEY_FILE ${KEY_FILE}.pub"
