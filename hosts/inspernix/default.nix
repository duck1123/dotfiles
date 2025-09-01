{ system, identities, ... }: {
  inherit system;
  id = "OWMQLRL-CD5VB7H-A3T436E-6XT4H66-6XRF22Y-MQXMNAU-DFRNGOV-ADSKFAV";
  identity = identities.duck;
  name = "inspernix";
  hostname = "inspernix";

  features = {
    backups.enable = false;
    battery.enable = true;
    bitcoin.enable = false;
    bluetooth.enable = true;
    chm.enable = false;
    clojure.enable = true;
    dbt.enable = false;
    dconf.enable = false;
    developer.enable = false;
    docker.enable = true;
    dunst.enable = false;
    emacs.enable = true;
    emacs-prelude.enable = false;
    email.enable = false;
    font.enable = true;
    gaming.enable = true;
    git.enable = true;
    gnome.enable = true;
    flipper.enable = false;
    hyprland.enable = true;
    hyprpanel.enable = true;
    i3.enable = false;
    java.enable = true;
    jujutsu.enable = true;

    kubernetes = {
      client.enable = true;
      server.enable = false;
    };

    media = {
      enable = false;
      server.enable = false;
    };

    music.enable = false;
    ncmpcpp.enable = false;
    network.enable = true;
    nfs.enable = true;
    nix.enable = true;
    nostr.enable = true;
    nushell.enable = true;
    office.enable = false;
    pictures.enable = false;
    radio.enable = false;
    sddm.enable = true;
    sound.enable = true;
    ssh.enable = true;
    stylix.enable = true;

    syncthing = {
      enable = true;

      shares = {
        camera.enable = false;
        keepass.enable = true;
        org-roam.enable = true;
        renpy.enable = true;
      };
    };

    tailscale.enable = true;
    touch.enable = true;
    vim.enable = false;
    virtualization.enable = false;
    vscode.enable = true;
    waybar.enable = false;
    xserver.enable = true;
    zsh.enable = true;
  };

  nixos = {
    enable = true;
    budgie.enable = false;
    gnome.enable = false;
    hyprland.enable = false;
    i3.enable = false;
    plasma6.enable = false;
  };
}
