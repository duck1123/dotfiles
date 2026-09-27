{ secrets, ... }:
{
  services.cloudflared = {
    enable = true;

    tunnelToken = secrets.cloudflared.tunnelToken;
  };
}
