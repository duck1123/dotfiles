_: {
  features.touch = {
    nixos = _: {
      services.libinput.enable = true;
    };
  };
}
