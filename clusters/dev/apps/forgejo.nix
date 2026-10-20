{ secrets, ... }:
{
  services.forgejo = {
    admin = { inherit (secrets.forgejo.admin) password username; };
    enable = false;
    databaseTarget = "postgresql";
    ingressProvider = "traefik-lan";
    monitoring.autokuma.enable = true;

    # The postgres login forgejo connects with (not forgejo's own admin user).
    database = {
      username = "postgres";
      password = secrets.forgejo.postgresql.adminPassword;
    };

    storageClassName = "longhorn";

    # Captured via `kubectl get pv <name> -o jsonpath='{.spec.csi.volumeHandle}'`
    # -- see docs/pinned-volumes.md. Specific to this cluster.
    volumeOverrides.data.volumeHandle = "pvc-13e105ec-412c-4937-a19c-1b385f026664";
  };
}
