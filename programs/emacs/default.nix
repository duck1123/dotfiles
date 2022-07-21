{
  # programs.emacs = {
  #   enable = true;
  # };

  home.file.".emacs.d/personal/01-personal.el".source = ./01-personal.el;
  home.file.".emacs.d/personal/prelude-modules.el".source = ./prelude-modules.el;
  home.file.".emacs.d/personal/ag-and-a-half/ag-and-a-half.el".source = ./ag-and-a-half/ag-and-a-half.el;

  # home.file.".emacs/init.el".text = ''
  #   (load "default.el")
  # '';
}
