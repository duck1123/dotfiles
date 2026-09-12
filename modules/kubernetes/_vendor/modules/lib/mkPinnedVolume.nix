{ ... }:
{
  # mkPinnedVolume
  #
  # Returns a { persistentVolumeClaims; persistentVolumes; } fragment binding
  # `pvcName` to a fixed-name PersistentVolume whose csi.volumeHandle is an
  # *existing* Longhorn volume's real identity, rather than letting the
  # StorageClass dynamically provision a fresh (empty) one.
  #
  # Why: this repo's apps go through `enable = false` -> `enable = true`
  # cycles via ArgoCD's prune + foreground-finalizer cascade, which deletes
  # PVCs (and any hand-authored PVs) outright on disable. The underlying
  # Longhorn volume survives regardless (the cluster's "longhorn"
  # StorageClass already sets reclaimPolicy: Retain), but a fresh dynamic
  # PVC on re-enable has no way to find that orphaned volume again -- it
  # just gets a new empty one. Pinning the PVC to a PV with a known,
  # unchanging volumeHandle fixes that: the PV/PVC objects are free to be
  # deleted and recreated every cycle, but volumeHandle always points at the
  # same backing data.
  #
  # `volumeHandle` must be captured once from the live cluster (`kubectl get
  # pv <name> -o jsonpath='{.spec.csi.volumeHandle}'`) before first use --
  # this function has no way to look it up itself.
  #
  # Both objects carry `Replace=true` only -- NOT `Force=true`. Force=true
  # was tried (see git history) specifically to let ArgoCD self-heal the one
  # genuinely-immutable transition (switching an already-bound PVC from a
  # dynamic PV to this one changes `volumeName`/`storageClassName`, which
  # Kubernetes rejects on a plain replace with "spec is immutable after
  # creation") without a manual delete. In practice it caused far more
  # disruption than it solved: during the mariadb bitnami->groundhog2k
  # migration (2026-09-07), Force=true triggered a delete+recreate of the
  # pinned PVC/PV on effectively *any* full sync of the app -- not just when
  # the pin itself changed -- repeatedly tearing down a volume actively
  # mounted by a running pod and leaving it stuck (Terminating, or Released
  # with a stale claimRef) until manually fixed. Plain `Replace=true` doesn't
  # do this: without Force, a genuine immutable-field mismatch just retries
  # forever harmlessly (no delete, no downtime) instead of resolving itself,
  # which means the one-time pin/unpin transition still needs the same
  # manual step this was meant to avoid -- scale the app to 0, delete the
  # stale PVC by hand, let it recreate against the same volumeHandle -- but
  # that manual step is one-time and predictable, unlike Force=true's
  # unpredictable recurring churn. This only ever lands on the PVC/PV a
  # pinned volume generates -- unpinned/dynamic volumes get no sync-options
  # annotation at all, and it can't leak onto any other resource in the app.
  #
  # `storageClassName` defaults to "" (fully static, no provisioner
  # involvement -- the safest default, and what every currently-pinned
  # volume in this repo uses). Longhorn's admission webhook refuses to
  # expand any volume whose PVC has storageClassName "" ("only dynamically
  # provisioned pvc can be resized" -- see longhorn/longhorn#6446), even
  # though `volumeName` still binds statically regardless of
  # storageClassName. Pass storageClassName = "longhorn" for a volume that
  # may need `kubectl patch volumes.longhorn.io ... spec.size` resizing
  # later -- but note this requires a delete+recreate of the PVC if it was
  # already provisioned with the "" default, since storageClassName is
  # immutable on an existing PVC.
  flake.lib.mkPinnedVolume =
    {
      pvcName,
      volumeHandle,
      size,
      pvName ? "${pvcName}-pv",
      accessModes ? [ "ReadWriteOnce" ],
      volumeAttributes ? { },
      storageClassName ? "",
    }:
    {
      persistentVolumeClaims.${pvcName} = {
        metadata.annotations."argocd.argoproj.io/sync-options" = "Replace=true";
        spec = {
          inherit accessModes storageClassName;
          resources.requests.storage = size;
          volumeName = pvName;
        };
      };

      persistentVolumes.${pvName} = {
        apiVersion = "v1";
        kind = "PersistentVolume";
        metadata = {
          name = pvName;
          annotations."argocd.argoproj.io/sync-options" = "Replace=true";
        };
        spec = {
          capacity.storage = size;
          inherit accessModes storageClassName;
          persistentVolumeReclaimPolicy = "Retain";
          csi = {
            driver = "driver.longhorn.io";
            fsType = "ext4";
            inherit volumeHandle;
            volumeAttributes = {
              numberOfReplicas = "1";
              staleReplicaTimeout = "30";
              fromBackup = "";
              fsType = "ext4";
              dataLocality = "disabled";
              unmapMarkSnapChainRemoved = "ignored";
              disableRevisionCounter = "true";
              dataEngine = "v1";
            }
            // volumeAttributes;
          };
        };
      };
    };
}
