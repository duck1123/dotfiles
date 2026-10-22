{ secrets, ... }:
{
  services.cert-manager = {
    cloudflare.token = secrets.cloudflare.token;
    email = "duck@kronkltd.net";
  };
}
