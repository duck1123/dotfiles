{ config, lib, ... }: {
  config = lib.mkIf config.host.features.bitcoin.enable {
    services.bitcoind.main = {
      enable = true;
      # dataDir = "/mnt/data3/bitcoin/bitcoind";
    };
  };
}
