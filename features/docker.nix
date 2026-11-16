{

  homeManager =
    { config, pkgs, ... }:
    {
      home.packages = with pkgs; [
        # devpod
        # devpod-desktop
        docker
        # docker-compose
        # podman
        # podman-desktop
        # podman-tui
        # tilt
      ];
    };

  nixos =
    { config, ... }:
    {
      users.users.${config.host.identity.username}.extraGroups = [ "docker" ];
      virtualisation.docker.enable = true;
    };
}
