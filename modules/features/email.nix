_: {
  features.email = {
    homeManager =
      { pkgs, ... }:
      {
        home.packages = with pkgs; [ thunderbird ];
      };
  };
}
