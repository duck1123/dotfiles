{ pkgs, ... }:
let
  widevine = pkgs.fetchurl {
    url = "https://dl.google.com/widevine-cdm/4.10.2252.0-linux-x64.zip";
    sha256 = "<sha256-hash-of-the-zip-file>";
  };

  widevine-unzip = pkgs.stdenv.mkDerivation {
    name = "widevine-unzip";
    src = widevine;
    buildInputs = [ pkgs.unzip ];
    buildPhase = ''
      unzip $src -d $out
    '';
  };
in {
  imports = [
    # ../../programs/backups
    ../../programs/base
    ../../programs/clojure
    # ../../programs/dbt
    ../../programs/dconf
    ../../programs/developer
    # ../../programs/dunst
    ../../programs/emacs
    # ../../programs/emacs2
    ../../programs/gaming
    ../../programs/git
    ../../programs/gnome
    ../../programs/hyprland
    ../../programs/hyprpanel
    # ../../programs/i3
    ../../programs/jujutsu
    # ../../programs/music
    # ../../programs/ncmpcpp
    ../../programs/nostr
    ../../programs/nushell
    ../../programs/radio
    ../../programs/stylix
    # ../../programs/vim
    ../../programs/vscode
    ../../programs/waybar
    ../../programs/zsh
  ];

  home = {
    packages = with pkgs; [
      # aider-chat-full
      alacritty
      # chromium
      # cheese
      colmena
      # cloudflare-cli
      # cloudflared
      digikam
      discord
      distrobox
      docker
      # docker-compose
      # earthly
      # fastfetch
      ffmpeg
      # fish
      # gcc9
      gimp
      # gitu
      gnumeric
      # gpa
      # gnome.dconf-editor
      # gnome-photos
      # gnome-tweaks
      # gnupg
      # google-chrome
      # graphviz
      # gum
      jdk
      # kakoune
      # kb
      # keet
      # khoj
      kodi
      kty
      kubernix
      libnotify
      # logseq
      mdcat
      minio-client
      # mullvad-browser
      nerdfetch
      # nerd-fonts.fira-code
      networkmanager
      nix-tree
      # obsidian
      # onlyoffice-bin
      playerctl
      plex
      # plex-media-player
      # postman
      qFlipper
      # radicle-node
      # simplex-chat-desktop
      slack
      # sparrow
      syncthing
      tailscale
      tdesktop
      teams-for-linux
      thunderbird
      # tilt
      transmission_4-gtk
      # tree
      unzip
      # virtualbox
      vlc
      vscode
      wine
      xsel
      # yt-dlp
      # yq
      # zoom-us
    ];

    sessionPath = [ "$HOME/.cargo/bin:$PATH" "$HOME/.local/bin:$PATH" ];
  };

  programs.chromium = {
    enable = true;
    # extraPackages = [
    #   (pkgs.runCommand "chromium-with-widevine" {} ''
    #     mkdir -p $out/lib/chromium
    #     cp ${widevine-unzip}/libwidevinecdm.so $out/lib/chromium/
    #   '')
    # ];
    # settings = {
    #   # "enable-widevine" = true;
    #   # "widevine-path" = "${widevine-unzip}/libwidevinecdm.so";
    # };
  };
}
