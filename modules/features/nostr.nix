_: {
  features.nostr = {
    homeManager =
      { pkgs, ... }:
      {
        home.packages = with pkgs; [
          algia
          # gossip
          nak
          nostui
        ];
      };
  };
}
