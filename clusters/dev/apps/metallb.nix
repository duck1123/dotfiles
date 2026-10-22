_: {
  services.metallb = {
    l2.addresses = [ "192.168.0.240-192.168.0.250" ];
    l2.excludeNodes = [ "powerspecnix" ];
  };
}
