{ host, lib, pkgs, ... }: {
  options = {
    features.zsh.enable = lib.mkOption {
      type = lib.types.bool;
      default = false;
      description = "Enable zsh";
    };
  };

  config = lib.mkIf host.features.zsh.enable {
    environment.systemPackages = with pkgs; [ zsh ];
    programs.zsh.enable = true;
  };
}
