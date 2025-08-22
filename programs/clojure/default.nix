{ host, lib, pkgs, ... }: {
  config = lib.mkIf host.features.clojure.enable {
    home.packages = with pkgs; [ babashka bbin clojure clojure-lsp jet ];
  };
}
