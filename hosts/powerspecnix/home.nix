{ pkgs, ... }: {
  imports = [ ../../programs ];

  home = {
    packages = with pkgs; [
      alacritty
      colmena
      discord
      distrobox
      docker
      # fastfetch
      ffmpeg
      # gitu
      # kakoune
      # kb
      # keet
      # khoj
      kty
      libnotify
      # logseq
      # mdcat
      minio-client
      # mullvad-browser
      # nerd-fonts.fira-code
      networkmanager
      nix-tree
      # obsidian
      # onlyoffice-bin
      playerctl
      # postman
      # sparrow
      syncthing
      tailscale
      tdesktop
      # tilt
      transmission_4-gtk
      # tree
      unzip
      # virtualbox
      vscode
      wine
      xsel
      # yq
    ];

    sessionPath = [ "$HOME/.cargo/bin:$PATH" "$HOME/.local/bin:$PATH" ];
  };
}
