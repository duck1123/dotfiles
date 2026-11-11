{
  homeManager =
    {
      config,
      inputs,
      lib,
      pkgs,
      ...
    }:
    {
      home.packages =
        with pkgs;
        [
          algia
          # gossip
          nak
          nostui
        ]
        # ngit is git over nostr, so it needs both features.
        ++
          lib.optional config.host.features.git.enable
            inputs.self.packages.${pkgs.stdenv.hostPlatform.system}.ngit;
    };
}
