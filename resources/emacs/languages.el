(use-package go-mode :ensure t)

(use-package nix-mode :ensure t)

(use-package terraform-mode :ensure t)

(use-package typescript-ts-mode
  :mode (("\\.[cm]?ts\\'" . typescript-ts-mode)
         ("\\.tsx\\'" . tsx-ts-mode)))
