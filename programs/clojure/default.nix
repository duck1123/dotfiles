{ config, lib, pkgs, ... }: {
  config = lib.mkIf config.host.features.clojure.enable {
    home.packages = with pkgs; [ babashka bbin clojure clojure-lsp jet ];
  };
}
