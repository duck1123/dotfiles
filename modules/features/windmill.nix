_: {
  features.windmill = {
    homeManager =
      { inputs, pkgs, ... }:
      {
        home.packages = [ inputs.self.packages.${pkgs.stdenv.hostPlatform.system}.windmill-cli ];

        xdg.configFile."fish/completions/wmill.fish".source =
          pkgs.runCommand "wmill-fish-completions" { }
            ''
              ${
                inputs.self.packages.${pkgs.stdenv.hostPlatform.system}.windmill-cli
              }/bin/wmill completions fish > $out
            '';
      };
  };
}
