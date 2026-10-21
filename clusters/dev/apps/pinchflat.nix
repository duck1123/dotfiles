{ secrets, ... }:
{
  services.pinchflat = {
    enable = true;
    hostAffinity = "edgenix";

    ingressProvider = "traefik-lan";
    nfsTarget = "nas";
    nfsSubPath = "Pinchflat";
    nfs.enable = true;

    secretKeyBase = secrets.pinchflat.secretKeyBase;

    homepage.group = "Media";
  };
}
