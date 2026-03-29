{ ... }:
{
  flake.modules.nixos.state-version =
    { ... }:
    {
      system.stateVersion = "26.05";
    };

  flake.modules.homeManager.state-version =
    { ... }:
    {
      home.stateVersion = "21.11";
    };
}
