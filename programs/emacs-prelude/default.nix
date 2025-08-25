{ host, lib, pkgs, ... }: {
  config = lib.mkIf host.features.emacs-prelude.enable {
    home = {
      file = {
        ".emacs.d/personal/01-personal.el".source = ./01-personal.el;
        ".emacs.d/personal/prelude-modules.el".source = ./prelude-modules.el;
        ".emacs.d/personal/ag-and-a-half/ag-and-a-half.el".source =
          ./ag-and-a-half/ag-and-a-half.el;
      };

      packages = with pkgs; [ emacs ];
    };
  };
}
