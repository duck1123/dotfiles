{
  homeManager =
    { pkgs, ... }:
    {
      home.packages = with pkgs; [
        python3
      ];
    };
}
