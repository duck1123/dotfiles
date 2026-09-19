_: {
  features.battery = {
    nixos = _: {
      services.upower.enable = true;
    };
  };
}
