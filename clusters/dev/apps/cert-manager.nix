{ secrets, ... }:
{
  services.cert-manager = {
    enable = true;
    cloudflare.token = secrets.cloudflare.token;
    email = "duck@kronkltd.net";
  };
}
