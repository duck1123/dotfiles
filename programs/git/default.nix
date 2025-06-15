{ host, ... }:
let inherit (host.identity) email gpgKey name;
in {
  programs = {
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
}
