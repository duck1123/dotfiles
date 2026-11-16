{
  homeManager =
    { pkgs, ... }:
    {
      home.packages = with pkgs; [
        jdk
        # openjdk
        # openjdk17
      ];
    };
}
