#!/usr/bin/env nu
# Fixups for generated nixidy manifests that nixidy can't express itself
# (Prometheus admission webhook RBAC/Jobs, MetalLB webhook cert
# ignoreDifferences). Edits go through yq so untouched parts of each file keep
# their formatting.
#
# Usage: nu scripts/k8s-post-process-manifests.nu kubernetes/manifests/dev

# Run yq-go, falling back to nixpkgs when it isn't on PATH
def yq-edit [expr: string, file: path] {
  if (which yq | is-not-empty) {
    ^yq eval -i $expr $file
  } else {
    ^nix run nixpkgs#yq-go -- eval -i $expr $file
  }
}

def main [
  env_dir: path  # the environment's output directory, e.g. kubernetes/manifests/dev
] {
  let prom_dir = $env_dir | path join prometheus
  let apps_dir = $env_dir | path join apps

  # Prometheus admission webhook: give ArgoCD time to see the Jobs complete
  for job in [
    "Job-prometheus-kube-prometheus-admission-create.yaml"
    "Job-prometheus-kube-prometheus-admission-patch.yaml"
  ] {
    let file = $prom_dir | path join $job
    if ($file | path exists) {
      yq-edit '.spec.ttlSecondsAfterFinished = 300' $file
      print $"Patched ($job): ttlSecondsAfterFinished=300 \(gives ArgoCD time to see completion)"
    }
  }

  # Remove admission RBAC from manifests - they already exist in-cluster (from
  # prior Helm install). ArgoCD applying them causes "already exists" errors.
  for kind in [Role RoleBinding ClusterRole ClusterRoleBinding] {
    let file = $prom_dir | path join $"($kind)-prometheus-kube-prometheus-admission.yaml"
    if ($file | path exists) {
      rm $file
      print $"Removed ($file) \(already exists in cluster, skip to avoid apply conflict)"
    }
  }

  # RBAC + both webhook configs: caBundle is injected by the API server after
  # apply; ignore to avoid OutOfSync/sync failures
  let prom_app = $apps_dir | path join Application-prometheus.yaml
  if ($prom_app | path exists) {
    yq-edit '.spec.ignoreDifferences = [
      {"kind": "Role", "name": "prometheus-kube-prometheus-admission", "namespace": "prometheus"},
      {"kind": "RoleBinding", "name": "prometheus-kube-prometheus-admission", "namespace": "prometheus"},
      {"kind": "ClusterRole", "name": "prometheus-kube-prometheus-admission"},
      {"kind": "ClusterRoleBinding", "name": "prometheus-kube-prometheus-admission"},
      {"group": "admissionregistration.k8s.io", "kind": "ValidatingWebhookConfiguration", "name": "prometheus-kube-prometheus-admission", "jqPathExpressions": [".webhooks[].clientConfig.caBundle"]},
      {"group": "admissionregistration.k8s.io", "kind": "MutatingWebhookConfiguration", "name": "prometheus-kube-prometheus-admission", "jqPathExpressions": [".webhooks[].clientConfig.caBundle"]}
    ]' $prom_app
    print "Updated ignoreDifferences for Prometheus admission webhook RBAC and caBundle"
  }

  # MetalLB: webhook TLS is written into the Secret by the controller; Git
  # keeps an empty placeholder.
  let metallb_app = $apps_dir | path join Application-metallb.yaml
  if ($metallb_app | path exists) {
    yq-edit '.spec.ignoreDifferences = [
      {"group": "", "kind": "Secret", "name": "metallb-webhook-cert", "namespace": "metallb-system", "jsonPointers": ["/data"]}
    ]' $metallb_app
    print "Updated ignoreDifferences for MetalLB webhook cert Secret data"
  }
}
