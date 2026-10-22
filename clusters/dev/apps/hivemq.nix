_: {
  services.hivemq = {
    hostAffinity = "nixmini";

    serviceType = "LoadBalancer";
    storageClassName = "longhorn";
    # loadBalancerIP = "192.168.0.243";
  };
}
