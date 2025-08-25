{ host, lib, ... }: {
  config = lib.mkIf host.features.bitcoin.enable {
    services.bitcoind.main = {
      enable = true;
      dataDir = "/mnt/data3/bitcoin/bitcoind";
    };
  };
}
