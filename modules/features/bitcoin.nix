_: {
  features.bitcoin = {
    homeManager =
      { pkgs, ... }:
      {
        home.packages = with pkgs; [ sparrow ];
      };

    nixos = _: {
      services.bitcoind.main = {
        enable = true;
        # dataDir = "/mnt/data3/bitcoin/bitcoind";
      };
    };
  };
}
