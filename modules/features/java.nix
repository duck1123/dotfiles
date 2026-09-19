_: {
  features.java = {
    homeManager =
      { pkgs, ... }:
      {
        home.packages = with pkgs; [ jdk ];
      };
  };
}
