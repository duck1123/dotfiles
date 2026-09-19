_: {
  features.firefox = {
    homeManager =
      { config, ... }:
      {
        programs.firefox = {
          configPath = "${config.xdg.configHome}/mozilla/firefox";
          enable = true;
        };
      };

    nixos =
      { config, pkgs, ... }:
      {
        users.users."${config.host.identity.username}".packages = [ pkgs.firefox ];
      };
  };
}
