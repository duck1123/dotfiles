{ config, inputs, pkgs, ... }: {
  networking.hostName = "inspernix";
  networking.networkmanager.enable = true;

  time.timeZone = "America/Detroit";

  nix.extraOptions = ''
    experimental-features = nix-command flakes
  '';

  services = {
    gnome.gnome-keyring.enable = true;

    tailscale.enable = true;
    xserver.enable = true;

    # xserver.displayManager.gdm.enable = true;
    # xserver.desktopManager.gnome.enable = true;

    xserver.xkb = {
      layout = "us";
      variant = "";
    };
    printing.enable = true;

  };

  hardware.pulseaudio.enable = false;
  security.rtkit.enable = true;
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    pulse.enable = true;
  };

  users.users.duck = {
    isNormalUser = true;
    description = "Duck Nebuchadnezzar";
    extraGroups = [ "networkmanager" "wheel" ];
    packages = with pkgs; [ ];
  };

  programs.firefox.enable = true;

  programs = {
    dconf.enable = true;

    gnupg.agent = {
      enable = true;
      enableSSHSupport = true;
    };

    nix-ld = {
      enable = true;

      libraries = with pkgs; [ alsa-lib libGL renpy ];
    };

    steam.enable = true;

    zsh.enable = true;
  };

  services = {
    # Enable the OpenSSH daemon.
    openssh = {
      enable = true;

      settings = {
        KbdInteractiveAuthentication = false;
        PasswordAuthentication = false;
      };
    };

  };

  nixpkgs.config.allowUnfree = true;

  environment.systemPackages = with pkgs; [ ];
  system.stateVersion = "24.11";
}
