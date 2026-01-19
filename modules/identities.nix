{ ... }:
{
  flake.modules.generic.identities =
    { inputs, ... }:
    {
      imports = with inputs.self.modules.generic; [
        identity-duck
        identity-deck
        identity-drenfer
      ];
    };
}
