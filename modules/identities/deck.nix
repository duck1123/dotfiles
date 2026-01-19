{ ... }:
{
  flake.modules.generic.identity-deck =
    { config, ... }:
    {
      identities.deck = {
        inherit (config.identities.duck) email gpgKey name;
        username = "deck";
      };
    };
}
