{
  homeManager =
    { pkgs, ... }:
    {
      home.packages = with pkgs; [ qFlipper ];
    };

  nixos =
    { config, ... }:
    {
      hardware.flipperzero.enable = true;
      users.users.${config.host.identity.username}.extraGroups = [ "plugdev" ];
    };
}
