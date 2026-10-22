{ secrets, ... }:
{
  services.metabase = {
    ingressProvider = "traefik-lan";
    homepage.group = "Database";

    # Flip these on by populating metabase.admin.password in secrets.enc.yaml (see
    # `nur secrets edit`) -- until then the register-connections job is skipped
    # entirely (see appTemplates/metabase.nix).
    admin = {
      email = (secrets.metabase or { }).admin.email or "admin@metabase.local";
      password = (secrets.metabase or { }).admin.password or "";
    };
  };
}
