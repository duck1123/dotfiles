{ config, inputs, ... }:
let
  fullName = config.fullName;
  hostname = config.hostname;
  pkgs = inputs.pkgs;
  username = config.username;
in {
  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users."${username}" = {
    isNormalUser = true;
    description = fullName;
    extraGroups = [
      "dialout"
      "docker"
      "jackaudio"
      "libvirtd"
      "networkmanager"
      "plugdev"
      "realtime"
      "wheel"
    ];
    packages = with pkgs; [ appimage-run emacs firefox ];
    shell = pkgs.zsh;
  };
}
