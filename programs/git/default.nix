{ config, ... }: {
  programs = {
    git = with config; {
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
      settings.user = { inherit (config) name email; };
    };
  };
}
