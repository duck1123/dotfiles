_: {
  features.obsidian = {
    homeManager =
      { pkgs, ... }:
      {
        home.packages = with pkgs; [ obsidian ];
      };
  };
}
