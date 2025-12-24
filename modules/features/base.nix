{ ... }:
let unified-modules = [ "bitcoin" "chat" "network" "syncthing" ];
in {
  flake.types.generic.feature-options.base = { inputs, lib }:
    let inherit (inputs.self.types.generic) simpleFeature;
    in simpleFeature { inherit inputs lib; } "base feature";

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
      ]) ++ (map (name: inputs.self.modules.homeManager.${name}) unified-modules) ++ [
        inputs.self.modules.generic.options
        inputs.stylix.homeModules.stylix
        inputs.zen-browser.homeModules.beta
      ];
    };

    nixos.base = { inputs, ... }: {
      imports = (with inputs.self.modules.nixos; [
        battery-feature
        bluetooth-feature
        boot
        docker-feature
        flipper
        font-feature
        gaming-feature
        i18n
        kubernetes-feature
        nfs-feature
        nix-feature
        radio
        samba
        sddm
        sddm-feature
        sound-feature
        ssh-feature
        stylix-feature
        tailscale-feature
        touch-feature
        users
        virtualization-feature
        xserver-feature
        zsh-feature
      ]) ++ (map (name: inputs.self.modules.nixos.${name}) unified-modules) ++ [
        inputs.home-manager.nixosModules.home-manager
        inputs.sddm-sugar-candy-nix.nixosModules.default
        inputs.self.modules.generic.options
        inputs.sops-nix.nixosModules.sops
        inputs.stylix.nixosModules.stylix
      ];
    };
  };
}
