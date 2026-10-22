{ secrets, ... }:
{
  services.cloudflared = {
    allowControlPlane = true;

    tunnelToken = secrets.cloudflared.tunnelToken;
  };
}
