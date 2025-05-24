{ pkgs, ... }: {
  environment.systemPackages = with pkgs; [ libgtop wl-clipboard ];

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
    power-profiles-daemon.enable = true;
    printing.enable = true;
  };

  system.stateVersion = "24.11";

  time.timeZone = "America/Detroit";
}
