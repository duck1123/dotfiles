_: {
  features.docker = {
    nixos =
      { config, ... }:
      {
        users.users.${config.host.identity.username}.extraGroups = [ "docker" ];
        virtualisation.docker.enable = true;
      };
  };
}
