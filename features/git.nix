{
  homeManager =
    { config, pkgs, ... }:
    {
      home.packages = with pkgs; [
        # gitu
        gh
        gh-cal
        gh-dash
        gh-f
        # mr
      ];

      programs =
        let
          inherit (config.host.identity) email gpgKey name;
        in
        {
          git = {
            enable = true;

            lfs.enable = true;

            settings = {
              fetch.prune = true;
              user = { inherit email name; };
            };

            signing = {
              format = "openpgp";
              signByDefault = false;
              key = gpgKey;
            };
          };
          jujutsu = {
            enable = true;
            settings.user = { inherit name email; };
          };
        };
    };
}
