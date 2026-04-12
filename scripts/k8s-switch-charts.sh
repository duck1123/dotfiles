#!/usr/bin/env bash
# Build nixidy kubernetes manifests and write them to the kubernetes/manifests checkout.
#
# Prerequisites:
#   - kubernetes/manifests/ must be a checkout of the private manifests repo
#   - SOPS_AGE_KEY_FILE (or standard sops auth) must be set to decrypt secrets/k8s.enc.yaml
#
# Usage:
#   ./scripts/k8s-switch-charts.sh
#   SHOW_TRACE=true ./scripts/k8s-switch-charts.sh
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
DOTFILES_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
MANIFESTS_DIR="$DOTFILES_ROOT/kubernetes/manifests"
SYSTEM="${SYSTEM:-x86_64-linux}"
SHOW_TRACE="${SHOW_TRACE:-false}"

if [[ ! -d "$MANIFESTS_DIR/.git" ]]; then
  echo "ERROR: $MANIFESTS_DIR is not a git repository." >&2
  echo "Clone your private manifests repo there first:" >&2
  echo "  git clone <your-private-manifests-repo> kubernetes/manifests" >&2
  exit 1
fi

# ---------------------------------------------------------------------------
# 1. Decrypt kubernetes secrets to a temp file
# ---------------------------------------------------------------------------
TMP="$(mktemp)"
trap 'rm -f "$TMP"' EXIT

sops --decrypt "$DOTFILES_ROOT/secrets/k8s.enc.yaml" > "$TMP"
export DECRYPTED_SECRET_FILE="$TMP"

# ---------------------------------------------------------------------------
# 2. Build nixidy activation package (--impure reads DECRYPTED_SECRET_FILE)
# ---------------------------------------------------------------------------
EXTRA_ARGS=()
if [[ "$SHOW_TRACE" == "true" ]]; then
  EXTRA_ARGS+=(--show-trace)
fi

echo "Building nixidy activation package..."
OUT_PATHS="$(mktemp)"
trap 'rm -f "$TMP" "$OUT_PATHS"' EXIT
(
  cd "$DOTFILES_ROOT"
  nix run nixpkgs#nix-output-monitor -- build \
    ".#nixidyEnvs.${SYSTEM}.dev.activationPackage" \
    --impure \
    --no-link \
    --print-out-paths \
    "${EXTRA_ARGS[@]}"
) 2>&1 | tee >(grep '^/nix/store' > "$OUT_PATHS")

DRV_PATH="$(grep '^/nix/store' "$OUT_PATHS" | tail -1)"

if [[ -z "$DRV_PATH" ]]; then
  echo "ERROR: failed to build activation package" >&2
  exit 1
fi

# ---------------------------------------------------------------------------
# 3. Run activation from within the manifests repo checkout
#    nixidy writes to rootPath ("dev/") relative to the git root it finds.
# ---------------------------------------------------------------------------
echo "Activating manifests in $MANIFESTS_DIR ..."
cd "$MANIFESTS_DIR"
"$DRV_PATH/activate"

# ---------------------------------------------------------------------------
# 4. Write SopsSecret YAML manifests (encryption happens here, not in Nix)
# ---------------------------------------------------------------------------
echo "Writing sops secrets..."
cd "$DOTFILES_ROOT"
./scripts/k8s-write-sops-secrets.sh

echo "Done. Run 'bb k8s-push' to commit and push manifests."
