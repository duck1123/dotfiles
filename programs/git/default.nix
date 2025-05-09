{ identity, ... }: {
  programs = {
    git = with identity; {
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
      settings.user = { inherit (identity) name email; };
    };
  };
}
