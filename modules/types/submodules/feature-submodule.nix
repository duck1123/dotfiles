{ ... }: {
  flake.types.generic.feature-submodule = { inputs, lib, ... }:
    with lib;
    let
      inherit (inputs.self.types) generic;
      simpleFeature = generic.simpleFeature { inherit inputs lib; };
      media-submodule = generic.media-submodule { inherit inputs lib; };
      syncthing-submodule = generic.syncthing-submodule { inherit inputs lib; };
    in types.submodule {
      options = {
        backups = simpleFeature "backups feature";
        base = simpleFeature "base feature";
        battery = simpleFeature "battery feature";
        bitcoin = simpleFeature "bitcoin feature";
        bluetooth = simpleFeature "bluetooth feature";
        chm = simpleFeature "chm feature";
        clojure = simpleFeature "clojure feature";
        common = simpleFeature "common feature";
        dbt = simpleFeature "dbt feature";
        dconf = simpleFeature "dconf feature";
        developer = simpleFeature "developer feature";
        docker = simpleFeature "docker feature";
        dunst = simpleFeature "dunst feature";
        emacs = simpleFeature "emacs feature";
        emacs-prelude = simpleFeature "emacs-prelude feature";
        email = simpleFeature "email feature";
        font = simpleFeature "font feature";
        gaming = simpleFeature "gaming feature";
        git = simpleFeature "git feature";
        gnome = simpleFeature "gnome feature";
        flipper = simpleFeature "flipper feature";
        hyprland = simpleFeature "hyprland feature";
        hyprpanel = simpleFeature "hyprpanel feature";
        i3 = simpleFeature "i3 feature";
        java = simpleFeature "java feature";
        jujutsu = simpleFeature "jujutsu feature";

        kubernetes = mkOption {
          type = types.submodule {
            options = {
              client = simpleFeature "kubernetes client";
              server = simpleFeature "kubernetes server";
            };
          };
          default = { };
          description = "Kubernetes configuration";
        };

        media = mkOption {
          type = media-submodule;
          default = { };
          description = "Media configuration";
        };

        music = simpleFeature "music feature";
        ncmpcpp = simpleFeature "ncmpcpp feature";
        network = simpleFeature "network feature";
        nfs = simpleFeature "nfs feature";
        nix = simpleFeature "nix feature";
        nostr = simpleFeature "nostr feature";
        nushell = simpleFeature "nushell feature";
        office = simpleFeature "office feature";
        pictures = simpleFeature "pictures feature";
        radio = simpleFeature "radio feature";
        sddm = simpleFeature "sddm feature";
        sound = simpleFeature "sound feature";
        ssh = simpleFeature "ssh feature";
        stylix = simpleFeature "stylix feature";

        syncthing = mkOption {
          type = syncthing-submodule;
          default = { };
          description = "Syncthing configuration";
        };

        tailscale = simpleFeature "tailscale feature";
        touch = simpleFeature "touch feature";
        vim = simpleFeature "vim feature";
        virtualization = simpleFeature "virtualization feature";
        vscode = simpleFeature "vscode feature";
        waybar = simpleFeature "waybar feature";
        xserver = simpleFeature "xserver feature";
        zsh = simpleFeature "zsh feature";
      };
    };
}
