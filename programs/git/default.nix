{ host, lib, ... }: {
  config = lib.mkIf host.features.git.enable {
    programs = let inherit (host.identity) email gpgKey name;
    in {
      git = {
        enable = true;

        lfs.enable = true;

        settings = {
          user.name = "${name}";
          user.email = "${email}";
        };

        signing = {
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
