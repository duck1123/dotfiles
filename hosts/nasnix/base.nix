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

    zsh.enable = true;
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

    xserver = {
      enable = true;

      xkb = {
        layout = "us";
        variant = "";
      };
    };
  };

  system.stateVersion = "25.05";

  time.timeZone = "America/Detroit";

  users.users.duck = {
    isNormalUser = true;
    description = "Duck Nebuchadnezzar";
    extraGroups = [ "networkmanager" "wheel" "samba" ];
    packages = [ ];
  };

  environment.systemPackages = with pkgs; [ samba ];
}
