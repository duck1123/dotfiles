{ config, system, ... }: {
  inherit system;

  features = {
    backups.enable = true;
    battery.enable = false;
    bitcoin.enable = false;
    bluetooth.enable = true;
    chm.enable = false;
    clojure.enable = true;
    common.enable = true;
    dbt.enable = false;
    dconf.enable = false;
    developer.enable = true;
    docker.enable = true;
    dunst.enable = false;
    emacs.enable = true;
    emacs-prelude.enable = false;
    email.enable = true;
    flipper.enable = false;
    font.enable = true;
    gaming.enable = true;
    git.enable = true;
    gnome.enable = true;
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
      enable = true;
      server.enable = false;
    };

    music.enable = false;
    ncmpcpp.enable = false;
    network.enable = true;
    nfs.enable = false;
    nix.enable = true;
    nostr.enable = true;
    nushell.enable = true;
    office.enable = true;
    pictures.enable = false;
    radio.enable = false;
    sddm.enable = true;
    sound.enable = true;
    ssh.enable = true;
    starship.enable = true;
    stylix.enable = true;

    syncthing = {
      enable = true;

      shares = {
        camera.enable = true;
        keepass.enable = true;
        org-roam.enable = true;
        renpy.enable = true;
      };
    };

    tailscale.enable = true;
    touch.enable = false;
    vim.enable = false;
    virtualization.enable = false;
    vscode.enable = true;
    waybar.enable = false;
    xserver.enable = true;
    zsh.enable = true;
  };

  hostname = "powerspecnix";
  id = "UFCCQLJ-3EKBVCQ-O5CNVM5-ERJQAQG-JWKQRPU-7FOZHPG-VMEOMKJ-KZSUFQK";
  identity = config.identities.duck;
  name = "powerspecnix";
  nixos.enable = true;
}
