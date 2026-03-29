{ ... }:
{
  flake.modules.nixos.nixos =
    { ... }:
    {
      # Keep this module available for compatibility; stateVersion is provided via nixos.base
    };
}
