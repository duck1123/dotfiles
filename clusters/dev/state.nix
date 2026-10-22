{ lib, ... }:
let
  # The settings nur tasks change, kept in JSON so they can edit them:
  # <app>.enable, <app>.replicas and <app>.pins.<volume key> = the Longhorn
  # volume's name (its csi.volumeHandle, "pvc-<uuid>"; see
  # modules/kubernetes/docs/pinned-volumes.md). Each is set only here, never
  # in clusters/dev/apps/<app>.nix. An unknown app or key fails the build.
  state = lib.importJSON ./state.json;
  # Checked here too, not only by the option types: a disabled app's
  # replicas is never evaluated, so a bad value would otherwise pass.
  valid = {
    enable = lib.isBool;
    replicas = v: lib.isInt v && v >= 0;
    pins = v: lib.isAttrs v && lib.all lib.isString (lib.attrValues v);
  };
  check =
    app: s:
    let
      unknown = lib.subtractLists (lib.attrNames valid) (lib.attrNames s);
      invalid = lib.filter (k: !valid.${k} s.${k}) (lib.attrNames (builtins.intersectAttrs valid s));
    in
    lib.throwIf (unknown != [ ])
      "clusters/dev/state.json: ${app} has unknown key(s) ${lib.concatStringsSep ", " unknown} (allowed: ${lib.concatStringsSep ", " (lib.attrNames valid)})"
      (
        lib.throwIf (invalid != [ ])
          "clusters/dev/state.json: ${app} has invalid ${lib.concatStringsSep ", " invalid} (enable: true/false, replicas: >= 0, pins: { <volume key> = \"pvc-...\"; })"
          s
      );
in
{
  services = lib.mapAttrs (
    app: raw:
    let
      s = check app raw;
    in
    lib.optionalAttrs (s ? enable) { inherit (s) enable; }
    // lib.optionalAttrs (s ? replicas) { inherit (s) replicas; }
    // lib.optionalAttrs (s ? pins) {
      volumeOverrides = lib.mapAttrs (_: handle: { volumeHandle = handle; }) s.pins;
    }
  ) state;
}
