(defun efs/lsp-mode-setup ()
  (setq lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
  (lsp-headerline-breadcrumb-mode))

(use-package lsp-mode
  :ensure t
  :commands (lsp lsp-deferred)
  :hook
  ((js2-mode . lsp)
   (clojure-mode . lsp)
   (clojurescript-mode . lsp)
   (clojurec-mode . 'lsp)
   (nix-mode . 'lsp)
   (lsp-mode . efs/lsp-mode-setup))
  :init
  (setq lsp-keymap-prefix "C-c l"
        lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols)
        lsp-lens-enable t
        lsp-signature-auto-activate t)
  :config
  (lsp-enable-which-key-integration t))

(use-package helm-lsp
  :ensure t
  :after lsp-mode
  :commands helm-lsp-workspace-symbol)

(use-package lsp-ui
  :ensure t
  :after lsp-mode
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-ui-doc-position 'bottom))

(use-package lsp-treemacs
  :ensure t
  :after lsp-mode
  :commands lsp-treemacs-errors-list)

