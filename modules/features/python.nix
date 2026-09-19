_: {
  features.python.homeManager =
    { pkgs, ... }:
    {
      home.packages = with pkgs; [
        python3
      ];
    };
}
