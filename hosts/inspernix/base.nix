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
  };

  services = {
    gnome.gnome-keyring.enable = true;
    printing.enable = true;
  };

  time.timeZone = "America/Detroit";
}
