{ pkgs, ... }: {
  home.packages = with pkgs; [ babashka bbin clojure clojure-lsp jet ];
}
