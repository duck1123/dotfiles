{
  homeManager =
    { pkgs, ... }:
    {
      home.packages = with pkgs; [ jdk ];
    };
}
