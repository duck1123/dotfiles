{ config, ... }:
{
  inherit (config.identities.duck) email gpgKey name;
  username = "deck";
}
