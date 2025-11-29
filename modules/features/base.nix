{ ... }: {
  flake.types.generic.feature-options.base = { inputs, lib }:
    with lib;
    let
      inherit (inputs.self.types) generic;
      simpleFeature = generic.simpleFeature { inherit inputs lib; };
    in simpleFeature "base feature";

  flake.modules = {
    homeManager.base = { inputs, ... }: {
      imports = (with inputs.self.modules.homeManager; [
        backups
        chm
        clojure
        common
        dbt
        dconf
        developer
        dunst
        emacs
        emacs-prelude
        email
        flipper
        gaming
        git
        gnome
        hyprland
        hyprpanel
        i3
        java
        jujutsu
        media
        music
        ncmpcpp
        nostr
        nushell
        office
        pictures
        radio
        starship
        stylix
        vim
        vscode
        waybar
        zsh
      ]) ++ [
        inputs.self.modules.generic.options
        inputs.stylix.homeModules.stylix
        inputs.zen-browser.homeModules.beta
      ];
    };

    nixos.base = { inputs, ... }: {
      imports = (with inputs.self.modules.nixos; [
        battery-feature
        bitcoin-feature
        bluetooth-feature
        boot
        docker-feature
        flipper
        font-feature
        gaming-feature
        i18n
        kubernetes-feature
        media-feature
        network-feature
        nfs-feature
        nix-feature
        sddm
        sddm-feature
        sound-feature
        ssh-feature
        stylix-feature
        syncthing-feature
        tailscale-feature
        touch-feature
        users
        virtualization-feature
        xserver-feature
        zsh-feature
      ]) ++ [
        inputs.home-manager.nixosModules.home-manager
        inputs.sddm-sugar-candy-nix.nixosModules.default
        inputs.self.modules.generic.options
        inputs.sops-nix.nixosModules.sops
        inputs.stylix.nixosModules.stylix
      ];
    };
  };
}
