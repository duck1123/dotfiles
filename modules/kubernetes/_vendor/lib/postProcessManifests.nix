{ pkgs, ... }:
pkgs.writeShellScriptBin "post-process-manifests" ''
  set -euo pipefail

  # Prometheus admission webhook: patch Jobs and ensure RBAC stays (Role/RoleBinding grant secret create permission)
  PROM_MANIFESTS_DIR="manifests/dev/prometheus"
  for job in "Job-prometheus-kube-prometheus-admission-create.yaml" "Job-prometheus-kube-prometheus-admission-patch.yaml"; do
    if [ -f "$PROM_MANIFESTS_DIR/$job" ]; then
      ${pkgs.yq-go}/bin/yq eval -i '.spec.ttlSecondsAfterFinished = 300' "$PROM_MANIFESTS_DIR/$job"
      echo "Patched $job: ttlSecondsAfterFinished=300 (gives ArgoCD time to see completion)"
    fi
  done
  # Remove admission RBAC from manifests - they already exist in-cluster (from prior Helm install).
  # ArgoCD applying them causes "already exists" errors. The in-cluster RBAC is sufficient.
  for f in Role RoleBinding ClusterRole ClusterRoleBinding; do
    fpath="$PROM_MANIFESTS_DIR/''${f}-prometheus-kube-prometheus-admission.yaml"
    if [ -f "''$fpath" ]; then
      rm -f "''$fpath"
      echo "Removed ''$fpath (already exists in cluster, skip to avoid apply conflict)"
    fi
  done

  # Also add ignoreDifferences so Argo does not fight server-injected fields
  PROM_APP_FILE="manifests/dev/apps/Application-prometheus.yaml"
  if [ -f "$PROM_APP_FILE" ]; then
    # RBAC + both webhook configs: caBundle is injected by the API server after apply; ignore to avoid OutOfSync/sync failures
    ${pkgs.yq-go}/bin/yq eval -i '.spec.ignoreDifferences = [
      {"kind": "Role", "name": "prometheus-kube-prometheus-admission", "namespace": "prometheus"},
      {"kind": "RoleBinding", "name": "prometheus-kube-prometheus-admission", "namespace": "prometheus"},
      {"kind": "ClusterRole", "name": "prometheus-kube-prometheus-admission"},
      {"kind": "ClusterRoleBinding", "name": "prometheus-kube-prometheus-admission"},
      {"group": "admissionregistration.k8s.io", "kind": "ValidatingWebhookConfiguration", "name": "prometheus-kube-prometheus-admission", "jqPathExpressions": [".webhooks[].clientConfig.caBundle"]},
      {"group": "admissionregistration.k8s.io", "kind": "MutatingWebhookConfiguration", "name": "prometheus-kube-prometheus-admission", "jqPathExpressions": [".webhooks[].clientConfig.caBundle"]}
    ]' "$PROM_APP_FILE"
    echo "Updated ignoreDifferences for Prometheus admission webhook RBAC and caBundle"
  fi

  # MetalLB: webhook TLS is written into the Secret by the controller; Git keeps an empty placeholder.
  METALLB_APP_FILE="manifests/dev/apps/Application-metallb.yaml"
  if [ -f "$METALLB_APP_FILE" ]; then
    ${pkgs.yq-go}/bin/yq eval -i '.spec.ignoreDifferences = [
      {"group": "", "kind": "Secret", "name": "metallb-webhook-cert", "namespace": "metallb-system", "jsonPointers": ["/data"]}
    ]' "$METALLB_APP_FILE"
    echo "Updated ignoreDifferences for MetalLB webhook cert Secret data"
  fi
''
