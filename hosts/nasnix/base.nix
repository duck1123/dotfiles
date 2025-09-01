{ pkgs, ... }: {
  programs = {
    # dconf.enable = true;
    # firefox.enable = true;

    # gnupg.agent = {
    #   enable = true;
    #   enableSSHSupport = true;
    # };

    # nix-ld = {
    #   enable = true;
    #   libraries = with pkgs; [
    #     alsa-lib
    #     libGL
    #     # renpy
    #   ];
    # };
  };

  services = {
    # gnome.gnome-keyring.enable = true;

    # Enable Samba client for Windows/SMB shares
    samba = {
      enable = true;
      settings = {
        global = {
          security = "user";
          "client min protocol" = "SMB2";
          "client max protocol" = "SMB3";
          "workgroup" = "WORKGROUP";
        };
      };
    };

    # printing.enable = true;
  };

  system.stateVersion = "25.05";
  time.timeZone = "America/Detroit";
  environment.systemPackages = with pkgs; [ samba ];
}
