(setq load-package-verbose t)
(setq gc-cons-threshold (* 100 1024 1024))
(setq read-process-output-max (* 1024 1024))
(setq create-lockfiles nil)
(setq make-backup-files nil)
(setq ring-bell-function 'ignore)

;; Change the user-emacs-directory to keep unwanted things out of ~/.emacs.d
(setq user-emacs-directory (expand-file-name "~/.cache/emacs/"))
(setq url-history-file (expand-file-name "url/history" user-emacs-directory))

;; Keep customization settings in a temporary file (thanks Ambrevar!)
(setq custom-file
  (if (boundp 'server-socket-dir)
      (expand-file-name "custom.el" server-socket-dir)
    (expand-file-name (format "emacs-custom-%s.el" (user-uid)) temporary-file-directory)))
(load custom-file t)

(setopt use-short-answers t)
(set-default-coding-systems 'utf-8)
(server-start)

(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(smartparens-global-mode +1)

(windmove-default-keybindings)

(use-package editorconfig
  :ensure t
  :config
  (editorconfig-mode 1))

(use-package general
  :ensure t
  :config
  (general-define-key
    "C-x C-k" 'kill-this-buffer
    "C-x b"   'helm-mini))

(use-package nyan-mode
  :ensure t
  :config
  (nyan-mode))

(use-package rainbow-delimiters
  :ensure t
  :init
  (rainbow-delimiters-mode-enable))

(global-set-key (kbd "C-S-<right>") 'sp-forward-slurp-sexp)
(global-set-key (kbd "C-M-s") 'sp-unwrap-sexp)
