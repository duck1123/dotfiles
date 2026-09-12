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
  echo "Run 'nur k8s manifests init' first (or 'git clone git@github.com:duck1123/argo-manifests.git kubernetes/manifests')." >&2
  exit 1
fi

SOPS=(sops)
if ! command -v sops >/dev/null 2>&1; then
  SOPS=(nix run nixpkgs#sops --)
fi

# ---------------------------------------------------------------------------
# 1. Decrypt kubernetes secrets to a temp file
# ---------------------------------------------------------------------------
TMP="$(mktemp)"
trap 'rm -f "$TMP"' EXIT

"${SOPS[@]}" --decrypt "$DOTFILES_ROOT/secrets/k8s.enc.yaml" > "$TMP"
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

# ---------------------------------------------------------------------------
# 5. Post-process manifests (fixups for nixidy hardcoded behaviours -- e.g.
#    Prometheus admission webhook RBAC/Jobs, MetalLB webhook cert
#    ignoreDifferences). Built from the k3s-fleetops flake input, same as
#    the library code applications/*.nix pull in.
#
#    post-process-manifests hardcodes fleetops' own "manifests/dev/..."
#    paths (its rootPath is "./manifests/dev"; ours is just "dev", no
#    "manifests/" wrapper -- see modules/kubernetes/_env/dev.nix). Rather
#    than fork the script or leave a permanent symlink in the checkout,
#    run it from a throwaway dir with a transient "manifests" symlink
#    pointing at the real checkout, so its relative paths resolve through
#    to the real files without touching what gets committed.
# ---------------------------------------------------------------------------
echo "Post-processing manifests..."
FLEETOPS_PATH="$(nix eval --raw --impure --expr 'let flake = builtins.getFlake (toString ./.); in flake.inputs.k3s-fleetops.outPath')"
POST_PROCESS_BIN="$(
  nix build --no-link --print-out-paths --impure --expr \
    "let flake = builtins.getFlake (toString ./.); pkgs = flake.inputs.nixpkgs.legacyPackages.${SYSTEM}; in pkgs.callPackage ${FLEETOPS_PATH}/lib/postProcessManifests.nix { }"
)"
POST_PROCESS_WRAPPER="$(mktemp -d)"
trap 'rm -f "$TMP" "$OUT_PATHS"; rm -rf "$POST_PROCESS_WRAPPER"' EXIT
ln -s "$MANIFESTS_DIR" "$POST_PROCESS_WRAPPER/manifests"
(cd "$POST_PROCESS_WRAPPER" && "$POST_PROCESS_BIN/bin/post-process-manifests")

echo "Done. Run 'bb k8s-push' to commit and push manifests."
