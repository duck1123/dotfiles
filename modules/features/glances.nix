_: {
  features.glances = {
    nixos = _: {
      services.glances = {
        enable = true;
        openFirewall = true;
      };
    };
  };
}
