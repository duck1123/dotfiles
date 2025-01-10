{ pkgs, ... }: {
  home.packages = with pkgs; [
    # CLI application for nostr
    algia

    # Desktop client for nostr, an open social media protocol
    gossip

    # Command-line tool for Nostr things
    nak

    # TUI client for Nostr
    nostui
  ];
}
