_: {
  flake.modules.nixos.state-version = _: {
    system.stateVersion = "26.05";
  };

  flake.modules.homeManager.state-version = _: {
    home.stateVersion = "21.11";
  };
}
