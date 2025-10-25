{ host, lib, ... }: {
  options = {
    features.bitcoin.enable = lib.mkOption {
      type = lib.types.bool;
      default = false;
      description = "Enable bitcoin";
    };
  };

  config = lib.mkIf host.features.bitcoin.enable {
    services.bitcoind.main = {
      enable = true;
      # dataDir = "/mnt/data3/bitcoin/bitcoind";
    };
  };
}
