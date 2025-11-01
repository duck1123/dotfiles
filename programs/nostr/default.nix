{ config, lib, pkgs, ... }: {
  config = lib.mkIf config.host.features.nostr.enable {
    home.packages = with pkgs; [
      # CLI application for nostr
      algia

      # Desktop client for nostr, an open social media protocol
      # gossip

      # Command-line tool for Nostr things
      nak

      # TUI client for Nostr
      nostui
    ];
  };
}
