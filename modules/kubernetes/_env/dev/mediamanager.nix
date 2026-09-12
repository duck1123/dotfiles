{ secrets, ... }:
{
  services.mediamanager = {
    enable = false;

    databaseTarget = "postgresql";
    database.enable = true;
    # Dedicated alphanumeric-only password -- see env/dev/securo.nix for why
    # an Alembic-migrated app can't safely share the punctuation-heavy shared
    # databaseProviders.postgresql password.
    database.password = secrets.mediamanager.database.password;

    tokenSecret = secrets.mediamanager.auth.tokenSecret;

    adminEmails = [ "duck@kronkltd.net" ];

    ingressProvider = "traefik-lan";
    ingress.tls.enable = true;
    homepage.group = "Arr";

    nfsTarget = "nas";
    nfs.enable = true;
  };
}
