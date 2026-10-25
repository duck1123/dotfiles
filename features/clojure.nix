{
  homeManager =
    { pkgs, ... }:
    {
      home.packages = with pkgs; [
        babashka
        bbin
        clojure
        clojure-lsp
        jet
      ];

      programs.emacs.parts.clojure = {
        order = 350;
        config = builtins.readFile ../resources/emacs/clojure.el;
      };
    };
}
