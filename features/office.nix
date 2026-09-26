{
  homeManager =
    { pkgs, ... }:
    {
      home.packages = with pkgs; [
        # gnumeric
        teams-for-linux
        # zoom-us
      ];
    };
}
