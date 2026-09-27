{ secrets, ... }:
{
  services.gluetun = {
    controlServer = { inherit (secrets.gluetun) password username; };
    enable = true;
    # hostAffinity = "edgenix";
    wireguardPrivateKey = secrets.mullvad.wireguardKey;
    wireguardAddresses = "10.69.161.167/32,fc00:bbbb:bbbb:bb01::6:a1a6/128";
    storageClassName = "longhorn";

    # Captured via `kubectl get pv <name> -o jsonpath='{.spec.csi.volumeHandle}'`
    # -- see docs/pinned-volumes.md. Specific to this cluster.
    volumeOverrides.gluetun.volumeHandle = "pvc-e563a1f5-1f3b-437e-ac58-77f4414da611";
  };
}
