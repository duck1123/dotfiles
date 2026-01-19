{ ... }:
{
  flake.modules.nixos.nixos =
    { ... }:
    {
      # Base nixos module - empty by default
      system.stateVersion = "26.05";
    };
}
