{ secrets, ... }:
{
  services.audiobookshelf = {
    enable = false;
    apiKey = secrets.audiobookshelf.key;
    # hostAffinity = "edgenix";

    ingressProvider = "traefik-lan";
    ingress.tls.enable = true;
    monitoring.autokuma.enable = true;
    homepage.group = "Media";

    nfsTarget = "nas";
    nfsSubPath = "Audiobooks";
    nfs.enable = true;

    # Captured via `kubectl get pv <name> -o jsonpath='{.spec.csi.volumeHandle}'`
    # -- see docs/pinned-volumes.md. Specific to this cluster.
    volumeOverrides = {
      config.volumeHandle = "pvc-fb2cd4cd-06ba-4dff-a6c5-7b7c2db91419";
      metadata.volumeHandle = "pvc-ce35f347-ab06-495f-88ae-11f7bcf4be8f";
    };
  };
}
