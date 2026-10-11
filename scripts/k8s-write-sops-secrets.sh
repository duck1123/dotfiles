#!/usr/bin/env bash
# Write SopsSecret YAML manifests to kubernetes/manifests/dev/<app>/ using sops.
#
# Adapted from k3s-fleetops/scripts/write-sops-secrets.sh for use from the dotfiles repo.
#
# Behaviour:
#   - Reuses committed ciphertext for unchanged secrets (no spurious git diffs).
#   - Encrypts fresh ciphertext for any secret that has no committed manifest.
#   - Deletes SopsSecret-*.yaml files for secrets that are no longer configured.
#
# Usage (standalone — handles its own decryption):
#   ./scripts/k8s-write-sops-secrets.sh
#
# nixidy's activation step (`activate`) rsyncs its build output over
# manifests/dev with --delete, and it has no idea SopsSecret files exist (they
# aren't part of the Nix build -- see mkArgoApp stripping sopsSecrets from
# resources). So by the time this script runs after activation, every
# SopsSecret-*.yaml in manifests/dev has already been wiped. Comparing against
# that live (post-delete) directory would always look "missing" and force a
# re-encrypt of everything. SOPS_SECRETS_REFERENCE_DIR lets the caller (see
# k8s-switch-charts.sh) point us at a pre-activation snapshot to diff/restore
# from instead; it defaults to MANIFESTS_DIR for standalone use, where nothing
# has deleted it out from under us.
#
# To force re-encryption of a secret, delete its manifest and commit that
# before switching, or it is simply restored from the snapshot.
#
# Required env / files:
#   secrets/k8s.enc.yaml  — sops-encrypted secrets file
#   SOPS_AGE_KEY_FILE or equivalent sops auth for decryption
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
DOTFILES_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
MANIFESTS_DIR="$DOTFILES_ROOT/kubernetes/manifests/dev"
REFERENCE_DIR="${SOPS_SECRETS_REFERENCE_DIR:-$MANIFESTS_DIR}"
SYSTEM="${SYSTEM:-x86_64-linux}"

SOPS=(sops)
if ! command -v sops >/dev/null 2>&1; then
  SOPS=(nix run nixpkgs#sops --)
fi

if [[ ! -d "$MANIFESTS_DIR" ]]; then
  echo "k8s-write-sops-secrets: $MANIFESTS_DIR not found, skipping" >&2
  echo "Run bb k8s-switch-charts first to generate manifests." >&2
  exit 0
fi

# ---------------------------------------------------------------------------
# 1. Decrypt secrets so nix eval --impure can read them via DECRYPTED_SECRET_FILE
# ---------------------------------------------------------------------------
TMP="$(mktemp)"
trap 'rm -f "$TMP"' EXIT

"${SOPS[@]}" --decrypt "$DOTFILES_ROOT/secrets/k8s.enc.yaml" > "$TMP"
export DECRYPTED_SECRET_FILE="$TMP"

# ---------------------------------------------------------------------------
# 2. Get secret specs (with plaintext values) via nix eval.
# ---------------------------------------------------------------------------
cd "$DOTFILES_ROOT"
SPECS_JSON="$(nix eval --impure --json ".#nixidySecretSpecs.${SYSTEM}.dev")"

AGE_RECIPIENTS="$(echo "$SPECS_JSON" | jq -r '.ageRecipients')"
if [[ -z "$AGE_RECIPIENTS" || "$AGE_RECIPIENTS" == "null" ]]; then
  echo "k8s-write-sops-secrets: ageRecipients not found in secret specs" >&2
  exit 1
fi

# ---------------------------------------------------------------------------
# 3. Build a list of desired output paths so we can clean up stale secrets
# ---------------------------------------------------------------------------
declare -A desired_files

while IFS= read -r spec; do
  secret_name="$(echo "$spec" | jq -r '.secretName')"
  app="$(echo "$spec" | jq -r '.app')"
  # Directory must match the app name, not the k8s namespace -- ArgoCD's
  # per-app Application syncs manifests/dev/<app>/, and those can differ
  # (e.g. app "longhorn" lives in namespace "longhorn-system"). A secret
  # written under the namespace name instead would silently never sync.
  desired_files["$MANIFESTS_DIR/$app/SopsSecret-${secret_name}.yaml"]=1
done < <(echo "$SPECS_JSON" | jq -c '.secrets[]')

# ---------------------------------------------------------------------------
# 4. Delete SopsSecret files for secrets that no longer exist in the config
# ---------------------------------------------------------------------------
while IFS= read -r existing; do
  if [[ -z "${desired_files[$existing]+_}" ]]; then
    echo "k8s-write-sops-secrets: removing stale secret: $existing"
    rm -f "$existing"
  fi
done < <(find "$MANIFESTS_DIR" -name "SopsSecret-*.yaml" 2>/dev/null)

# ---------------------------------------------------------------------------
# 5. Write each desired secret — skip if plaintext values unchanged
# ---------------------------------------------------------------------------
while IFS= read -r spec; do
  secret_name="$(echo "$spec" | jq -r '.secretName')"
  app="$(echo "$spec" | jq -r '.app')"
  namespace="$(echo "$spec" | jq -r '.namespace')"
  values="$(echo "$spec" | jq '.values')"
  output_file="$MANIFESTS_DIR/$app/SopsSecret-${secret_name}.yaml"
  reference_file="$REFERENCE_DIR/$app/SopsSecret-${secret_name}.yaml"

  # Check if a prior copy exists and compare plaintext values. Restore it
  # verbatim (reusing its ciphertext) rather than skipping outright -- the
  # activation rsync may have already deleted output_file, so "unchanged"
  # still needs to put the file back.
  if [[ -f "$reference_file" ]]; then
    existing_plaintext="$("${SOPS[@]}" --decrypt --input-type yaml --output-type json "$reference_file" | jq '.spec.secretTemplates[0].stringData')"
    if [[ "$existing_plaintext" == "$values" ]]; then
      echo "k8s-write-sops-secrets: reusing unchanged secret: $secret_name"
      if [[ ! "$reference_file" -ef "$output_file" ]]; then
        mkdir -p "$(dirname "$output_file")"
        cp "$reference_file" "$output_file"
      fi
      continue
    fi
  fi

  echo "k8s-write-sops-secrets: encrypting $secret_name"

  mkdir -p "$(dirname "$output_file")"

  # @json (not tostring/raw interpolation) is required: a value that is itself
  # a JSON string (e.g. superset/metabase's CONNECTIONS_JSON, a
  # builtins.toJSON-encoded list) starts with `[` or `{` and, embedded
  # unquoted, gets parsed by the YAML loader as a nested flow sequence/mapping
  # instead of staying a scalar string -- which desyncs the app in ArgoCD and
  # is unreadable by the container expecting a plain env var string.
  string_data_lines="$(echo "$values" | jq -r 'to_entries[] | "        \(.key): \(.value | @json)"')"

  metadata_yaml=""
  if echo "$spec" | jq -e '.metadata.annotations? != null' >/dev/null 2>&1; then
    metadata_yaml="metadata:
  annotations:
"
    metadata_yaml+="$(echo "$spec" | jq -c '.metadata.annotations' | python3 - <<'PY'
import json, sys
annotations = json.load(sys.stdin)
for key, value in annotations.items():
    print(f"    {key}: {json.dumps(value)}")
PY
)"
    metadata_yaml+=$'\n'
  fi

  plaintext_yaml="apiVersion: isindir.github.com/v1alpha3
kind: SopsSecret
metadata:
  name: ${secret_name}
  namespace: ${namespace}
${metadata_yaml}spec:
  secretTemplates:
    - name: ${secret_name}
      stringData:
${string_data_lines}"

  echo "$plaintext_yaml" \
    | "${SOPS[@]}" --encrypt \
        --age "$AGE_RECIPIENTS" \
        --encrypted-regex '^(stringData)$' \
        --input-type yaml \
        --output-type yaml \
        --filename-override secrets/k8s.enc.yaml \
        /dev/stdin \
    > "$output_file"

done < <(echo "$SPECS_JSON" | jq -c '.secrets[]')

echo "k8s-write-sops-secrets: done"
