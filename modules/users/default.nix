{ host, lib, pkgs, ... }:
let inherit (host.identity) name username;
in {
  options = {
    features.users.enable = lib.mkOption {
      type = lib.types.bool;
      default = false;
      description = "Enable users";
    };
  };

  config = lib.mkIf host.features.users.enable {
    # Define a user account. Don't forget to set a password with ‘passwd’.
    users.users."${username}" = {
      isNormalUser = true;
      description = name;
      extraGroups = [
        "dialout"
        "docker"
        "jackaudio"
        "libvirtd"
        "networkmanager"
        "plugdev"
        "realtime"
        "samba"
        "vboxusers"
        "wheel"
      ];
      packages = with pkgs; [ appimage-run emacs firefox ];
      shell = pkgs.zsh;
    };
  };
}
