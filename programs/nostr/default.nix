{ host, lib, pkgs, ... }: {
  config = lib.mkIf host.features.nostr.enable {
    home.packages = with pkgs; [
      algia
      # gossip
      nak
      nostui
    ];
  };
}
