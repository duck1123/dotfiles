{ secrets, ... }:
{
  services.windmill = {
    # hostAffinity = "nixmini";
    # Pinned to what `:latest` was actually running (2026-10-27). Upgrading
    # also means bumping wmill-cli (modules/kubernetes/pkgs/windmill-sync.nix).
    image = "ghcr.io/windmill-labs/windmill-full:1.687.0";

    ingressProvider = "traefik-lan";
    ingress.tls.enable = true;
    homepage.group = "Automation";

    databaseTarget = "postgresql";
    database = {
      inherit (secrets.windmill.database) username password;
    };

    storageClassName = "longhorn";

    # nfsTarget defaults to "nas" and nfsSubPath defaults to "", so this mounts
    # the whole nasnix media export at /media in both worker deployments --
    # the same library every *arr app sees.
    nfs.enable = true;

    inherit (secrets.windmill) superadminSecret superadminEmail superadminPassword;

    # Valueless definitions live next to each path under resources/apps/windmill/wmill/f/.
    secretVariables = [
      {
        path = "f/fleetops/youtube_api_key";
        value = secrets.youtube.apiKey;
      }
      {
        path = "f/fleetops/trilium_etapi_token";
        value = secrets.trilium.key;
      }
      {
        path = "f/phototags/immich_api_key";
        value = secrets.immich.phototagsApiKey;
      }
    ];
  };
}
