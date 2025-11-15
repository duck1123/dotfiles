{ ... }: {
  flake.modules.homeManager.emacs-prelude = { config, lib, pkgs, ... }: {
    config = lib.mkIf config.host.features.emacs-prelude.enable {
      home = {
        file = {
          ".emacs.d/personal/01-personal.el".source =
            ../../programs/emacs-prelude/01-personal.el;
          ".emacs.d/personal/prelude-modules.el".source =
            ../../programs/emacs-prelude/prelude-modules.el;
          ".emacs.d/personal/ag-and-a-half/ag-and-a-half.el".source =
            ../../programs/emacs-prelude/ag-and-a-half/ag-and-a-half.el;
        };

        packages = with pkgs; [ emacs ];
      };
    };
  };
}
