{ host, lib, pkgs, ... }: {
  options = {
    features.clojure.enable = lib.mkOption {
      type = lib.types.bool;
      default = false;
      description = "Enable clojure";
    };
  };

  config = lib.mkIf host.features.clojure.enable {
    home.packages = with pkgs; [ babashka bbin clojure clojure-lsp jet ];
  };
}
