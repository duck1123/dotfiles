{ secrets, ... }:
{
  services.cloudflared = {
    enable = true;
    allowControlPlane = true;

    tunnelToken = secrets.cloudflared.tunnelToken;
  };
}
