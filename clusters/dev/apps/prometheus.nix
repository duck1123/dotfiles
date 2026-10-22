_: {
  services.prometheus = {
    alertmanager.enabled = true;
    hostAffinity = "edgenix";
  };
}
