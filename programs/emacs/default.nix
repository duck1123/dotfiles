{ pkgs, ... }: {
  # programs.emacs = {
  #   enable = true;
  # };

  home = {
    file = {
      ".emacs.d/personal/01-personal.el".source = ./01-personal.el;
      ".emacs.d/personal/prelude-modules.el".source = ./prelude-modules.el;
      ".emacs.d/personal/ag-and-a-half/ag-and-a-half.el".source =
        ./ag-and-a-half/ag-and-a-half.el;

      # ".emacs/init.el".text = ''
      #   (load "default.el")
      # '';
    };

    packages = with pkgs; [ emacs ];
  };
}
