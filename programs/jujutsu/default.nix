{ host, lib, pkgs, ... }: {
  options = {
    features.jujutsu.enable = lib.mkOption {
      type = lib.types.bool;
      default = false;
      description = "Enable jujutsu";
    };
  };

  config = lib.mkIf host.features.jujutsu.enable {
    home.packages = with pkgs; [
      # gg-jj jj-fzf
      jjui
      jujutsu
      # lazyjj
    ];

    programs.jujutsu = {
      enable = true;
      settings.user = { inherit (host.identity) email name; };
    };
  };
}
