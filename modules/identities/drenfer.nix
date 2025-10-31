{ ... }: {
  flake.modules.generic.identity-drenfer = { config, ... }: {
    identities.drenfer = {
      inherit (config.identities.duck) gpgKey;
      email = "daniel.renfer@vallen.com";
      name = "Daniel E. Renfer";
      username = "drenfer";
    };
  };
}
