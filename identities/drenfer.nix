{ config, ... }:
{
  inherit (config.identities.duck) gpgKey;
  email = "daniel.renfer@vallen.com";
  name = "Daniel E. Renfer";
  username = "drenfer";
}
