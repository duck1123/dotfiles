{ ... }: {
  flake.modules.homeManager.nostr = { config, lib, pkgs, ... }: {
    config = lib.mkIf config.host.features.nostr.enable {
      home.packages = with pkgs; [
        algia
        # gossip
        nak
        nostui
      ];
    };
  };
}

