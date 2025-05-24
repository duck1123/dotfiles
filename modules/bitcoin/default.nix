{...}: {
  services.bitcoind.main = {
    enable = true;
    dataDir = "/mnt/data3/bitcoin/bitcoind";
  };
}
