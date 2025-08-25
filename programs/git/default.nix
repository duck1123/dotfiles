{ host, lib, ... }: {
  config = lib.mkIf host.features.git.enable {
    programs = let inherit (host.identity) email gpgKey name;
    in {
      git = {
        enable = true;
        userName = "${name}";
        userEmail = "${email}";
        lfs.enable = true;
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
