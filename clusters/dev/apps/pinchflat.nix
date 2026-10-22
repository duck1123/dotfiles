{ secrets, ... }:
{
  services.pinchflat = {
    hostAffinity = "edgenix";

    ingressProvider = "traefik-lan";
    nfsTarget = "nas";
    nfsSubPath = "Pinchflat";
    nfs.enable = true;

    secretKeyBase = secrets.pinchflat.secretKeyBase;

    homepage.group = "Media";
  };
}
