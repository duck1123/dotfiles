{ secrets, ... }:
{
  services.tailscale = {
    enable = true;
    oauth = { inherit (secrets.tailscale) authKey clientId clientSecret; };
  };
}
