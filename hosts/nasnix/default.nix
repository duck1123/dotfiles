{ system, identities, ... }: {
  inherit system;
  id = "WUCVTEF-D2NOIGW-IFJPFKD-RHT7NSP-CZSIWM7-KLCHS3S-EIO3WFD-6DGAVAN";
  identity = identities.duck;
  name = "nasnix";
  hostname = "nasnix";

  features = {
    backups.enable = false;
    bitcoin.enable = false;
    chm.enable = false;
    clojure.enable = true;
    dbt.enable = false;
    dconf.enable = false;
    developer.enable = true;
    dunst.enable = false;
    emacs.enable = true;
    emacs-prelude.enable = false;
    email.enable = false;
    flipper.enable = false;
    font.enable = true;
    gaming.enable = false;
    git.enable = true;
    gnome.enable = false;
    hyprland.enable = true;
    hyprpanel.enable = true;
    i3.enable = false;
    java.enable = false;
    jujutsu.enable = true;

    kubernetes = {
      client.enable = true;
      server.enable = true;
    };

    media = {
      enable = true;
      server.enable = true;
    };

    music.enable = false;
    ncmpcpp.enable = false;
    network.enable = true;
    nfs.enable = false;
    nix.enable = true;
    nostr.enable = true;
    nushell.enable = true;
    office.enable = false;
    pictures.enable = false;
    radio.enable = false;
    sddm.enable = true;
    stylix.enable = true;

    syncthing = {
      enable = true;

      shares = {
        camera.enable = false;
        keepass.enable = true;
        org-roam.enable = false;
        renpy.enable = false;
      };
    };

    vim.enable = false;
    virtualization.enable = false;
    vscode.enable = true;
    waybar.enable = false;
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
