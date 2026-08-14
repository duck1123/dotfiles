#!/usr/bin/env bash
# Commit and push generated manifests to the private manifests repo.
#
# Expects kubernetes/manifests/ to be a checkout of the private manifests repo.
# Run bb k8s-switch-charts first to generate/update the manifests.
#
# Usage:
#   ./scripts/k8s-push-manifests.sh
#   COMMIT_MSG="custom message" ./scripts/k8s-push-manifests.sh
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
DOTFILES_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
MANIFESTS_DIR="$DOTFILES_ROOT/kubernetes/manifests"

if [[ ! -d "$MANIFESTS_DIR/.git" ]]; then
  echo "ERROR: $MANIFESTS_DIR is not a git repository." >&2
  echo "Run 'nur k8s manifests init' first (or 'git clone git@github.com:duck1123/argo-manifests.git kubernetes/manifests')." >&2
  exit 1
fi

cd "$MANIFESTS_DIR"

git add .

if git diff --cached --quiet; then
  echo "No manifest changes to push."
  exit 0
fi

echo "Changes staged:"
git diff --cached --stat

COMMIT_MSG="${COMMIT_MSG:-Update manifests $(date -u +%Y-%m-%dT%H:%M:%SZ)}"
git commit -m "$COMMIT_MSG"
git push

echo "Manifests pushed successfully."
