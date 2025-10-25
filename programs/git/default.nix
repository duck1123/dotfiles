{ host, lib, ... }: {

  options = {
    features.git.enable = lib.mkOption {
      type = lib.types.bool;
      default = false;
      description = "Enable git";
    };
  };

  config = lib.mkIf host.features.git.enable {
    programs = let inherit (host.identity) email gpgKey name;
    in {
      git = {
        enable = true;

        lfs.enable = true;

        settings.user = { inherit email name; };

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
