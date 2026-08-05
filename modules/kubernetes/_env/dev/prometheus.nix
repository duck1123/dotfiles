{ ... }:
{
  services.prometheus = {
    alertmanager.enabled = true;
    enable = true;
    hostAffinity = "edgenix";
  };
}
