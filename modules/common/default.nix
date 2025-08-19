{ ... }: {
  hardware = {
    flipperzero.enable = true;
    rtl-sdr.enable = true;
  };

  programs = {
    dconf.enable = true;

    gnupg.agent = {
      enable = true;
      enableSSHSupport = true;
    };

    nix-ld = {
      enable = true;
      libraries = with pkgs; [ alsa-lib libGL ];
    };
  };

  services = {
    gnome.gnome-keyring.enable = true;
    printing.enable = true;
  };

  system.stateVersion = "25.05";
  time.timeZone = "America/Detroit";
}
