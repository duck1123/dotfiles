{ ... }:
{
  services.prometheus = {
    alertmanager.enabled = true;
    enable = false;
    hostAffinity = "edgenix";
  };
}
