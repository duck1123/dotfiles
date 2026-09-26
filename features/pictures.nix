{
  homeManager =
    { pkgs, ... }:
    {
      home.packages = with pkgs; [
        digikam
        gimp
        viewnior
      ];
    };
}
