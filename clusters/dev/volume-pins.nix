{ lib, ... }:
let
  # <app>.<volume key> = the Longhorn volume's name (its csi.volumeHandle),
  # e.g. "pvc-<uuid>". See modules/kubernetes/docs/pinned-volumes.md. JSON so
  # nur tasks can edit it; specific to this cluster.
  pins = lib.importJSON ./volume-pins.json;
in
{
  services = lib.mapAttrs (_: keys: {
    volumeOverrides = lib.mapAttrs (_: handle: { volumeHandle = handle; }) keys;
  }) pins;
}
