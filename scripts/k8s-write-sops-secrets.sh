#!/usr/bin/env bash
# Write SopsSecret YAML manifests to kubernetes/manifests/dev/<namespace>/ using sops.
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
# Required env / files:
#   secrets/k8s.enc.yaml  — sops-encrypted secrets file
#   SOPS_AGE_KEY_FILE or equivalent sops auth for decryption
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
DOTFILES_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
MANIFESTS_DIR="$DOTFILES_ROOT/kubernetes/manifests/dev"
SYSTEM="${SYSTEM:-x86_64-linux}"

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

sops --decrypt "$DOTFILES_ROOT/secrets/k8s.enc.yaml" > "$TMP"
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
  namespace="$(echo "$spec" | jq -r '.namespace')"
  desired_files["$MANIFESTS_DIR/$namespace/SopsSecret-${secret_name}.yaml"]=1
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
  namespace="$(echo "$spec" | jq -r '.namespace')"
  values="$(echo "$spec" | jq '.values')"
  output_file="$MANIFESTS_DIR/$namespace/SopsSecret-${secret_name}.yaml"

  if [[ -f "$output_file" ]]; then
    existing_plaintext="$(sops --decrypt --input-type yaml --output-type json "$output_file" | jq '.spec.secretTemplates[0].stringData')"
    if [[ "$existing_plaintext" == "$values" ]]; then
      echo "k8s-write-sops-secrets: skipping unchanged secret: $secret_name"
      continue
    fi
  fi

  echo "k8s-write-sops-secrets: encrypting $secret_name"

  string_data_lines="$(echo "$values" | jq -r 'to_entries[] | "        \(.key): \(.value | tostring)"')"

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
    | sops --encrypt \
        --age "$AGE_RECIPIENTS" \
        --encrypted-regex '^(stringData)$' \
        --input-type yaml \
        --output-type yaml \
        /dev/stdin \
    > "$output_file"

done < <(echo "$SPECS_JSON" | jq -c '.secrets[]')

echo "k8s-write-sops-secrets: done"
