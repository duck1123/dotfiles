{ system, identities, ... }: {
  inherit system;

  features = {
    backups.enable = false;
    bitcoin.enable = false;
    chm.enable = false;
    clojure.enable = true;
    dbt.enable = true;
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
    hyprland.enable = false;
    i3.enable = false;
    java.enable = true;
    jujutsu.enable = true;

    media = {
      enable = false;
      server.enable = false;
    };

    ncmpcpp.enable = false;
    nix.enable = true;
    nushell.enable = true;
    office.enable = false;
    pictures.enable = false;
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
    vscode.enable = false;
    waybar.enable = false;
    zsh.enable = true;
  };

  id = "TEED77K-QOLTQ37-BL76MFB-LJD46CW-EJ7CZTJ-7GQNEF6-FZAMQRP-BCCRTQ6";
  identity = identities.drenfer;
  name = "VallenPC";
  hostname = "vavirl-pw0bwnq8";
  home-manager.enable = true;
  nixos.enable = false;
}
