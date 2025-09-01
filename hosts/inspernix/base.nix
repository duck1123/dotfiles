{ pkgs, ... }: {
  programs = {
    dconf.enable = true;
    firefox.enable = true;

    gnupg.agent = {
      enable = true;
      enableSSHSupport = true;
    };

    nix-ld = {
      enable = true;
      libraries = with pkgs; [
        alsa-lib
        libGL
        # renpy
      ];
    };

    zsh.enable = true;
  };

  services = {
    gnome.gnome-keyring.enable = true;
    printing.enable = true;
  };

  system.stateVersion = "24.11";

  time.timeZone = "America/Detroit";
}
