;;; custom --- Custom code
;;; Commentary: This file is auto-generated. Edit the org file

;;; Code:

(setq prelude-guru nil)
(setq load-package-verbose t)

(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
        ("melpa-stable" . "https://stable.melpa.org/packages/")
        ("org" . "https://orgmode.org/elpa/")
        ("elpa" . "https://elpa.gnu.org/packages/")))

(setq gc-cons-threshold (* 100 1024 1024))
(setq read-process-output-max (* 1024 1024))

(prelude-require-packages
 '(crappy-jsp-mode
   editorconfig
   helm-projectile
   magit
   paredit
   prettier-js))

(global-set-key (kbd "C-x C-k")     'kill-this-buffer)

(setq create-lockfiles nil)
;; (setq make-backup-files nil)

;; Change the user-emacs-directory to keep unwanted things out of ~/.emacs.d
(setq user-emacs-directory (expand-file-name "~/.cache/emacs/")
      url-history-file (expand-file-name "url/history" user-emacs-directory))

;; Use no-littering to automatically set common paths to the new user-emacs-directory
;; (use-package no-littering
;;   :ensure true
;;   )

;; Keep customization settings in a temporary file (thanks Ambrevar!)
(setq custom-file
      (if (boundp 'server-socket-dir)
          (expand-file-name "custom.el" server-socket-dir)
        (expand-file-name (format "emacs-custom-%s.el" (user-uid)) temporary-file-directory)))
(load custom-file t)

(set-default-coding-systems 'utf-8)
(server-start)

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.3))


(use-package auto-complete
  :ensure t)

(use-package earthfile-mode :ensure t)
;; (use-package helm :ensure t)

(add-to-list 'load-path "~/.emacs.d/personal/ag-and-a-half")
(require 'ag-and-a-half)
(defalias 'ag 'ag-and-a-half)

(use-package helm-github-stars
  :ensure t
  :commands (helm-github-stars)
  :bind (("S-C-g" . helm-github-stars)))

(use-package nix-mode
  :ensure t)

(use-package nyan-mode
  :ensure t
  :config
  (nyan-mode))

(use-package rainbow-delimiters
  :ensure t
  :init
  (rainbow-delimiters-mode-enable))

(use-package scss-mode
  :ensure t)
(use-package docker :ensure t)

(use-package gradle-mode :ensure t)
(use-package flycheck-gradle
  :ensure t)

(use-package company
  :ensure t)

(use-package go-mode
  :ensure t)

(menu-bar-mode -1)
(scroll-bar-mode -1)
;; (add-to-list 'auto-mode-alist '("components\\/.*\\.js\\'" . rjsx-mode))
;; (setq tab-width 2)
;; (setq default-tab-width 2)

(use-package org-roam
  :ensure t

  :custom
  (org-roam-directory "~/Nextcloud/org-roam")
  (org-roam-capture-templates
   `(("d" "default" plain "%?"
      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
      :unnarrowed t)))

  :bind
  (("C-c n l" . org-roam-buffer-toggle)
   ("C-c n f" . org-roam-node-find)
   ("C-c n i" . org-roam-node-insert))

  :config
  (org-roam-setup)
  (org-roam-db-autosync-mode)
  (setq org-roam-dailies-capture-templates
        `(("d" "default" entry "* %?\n:PROPERTIES:\n:CREATED: %T\n:END:"
           :if-new (file+head "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>\n")))))

(use-package org-roam-ui
  :ensure t
  ;; :straight
  ;; (:host github :repo "org-roam/org-roam-ui" :branch "main" :files ("*.el" "out"))
  :after org-roam
  ;;  normally we'd recommend hooking orui after org-roam, but since org-roam does not have
  ;;  a hookable mode anymore, you're advised to pick something yourself
  ;;  if you don't care about startup time, use
  ;;  :hook (after-init . org-roam-ui-mode)
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start t))

(use-package magit-lfs
  :ensure t)

;; (add-hook 'java-mode-hook
;;           (lambda ()
;;             "Treat Java 1.5 @-style annotations as comments."
;;             (setq c-comment-start-regexp "(@|/(/|[*][*]?))")
;;             (modify-syntax-entry ?@ "< b" java-mode-syntax-table)))

(use-package flycheck-flow
  :ensure t)

(use-package typescript-mode
  :ensure t)

(use-package company-flow
  :ensure t)

;; (add-hook 'c-mode-common-hook (lambda () (c-set-offset 'case-label '+)))

;; (use-package kubernetes :ensure t)
;; (use-package kubernetes-helm :ensure t)
;; (require 'kubernetes)
;; (require 'kubernetes-helm)

(add-to-list 'auto-mode-alist '("\\.jsp\\'" . crappy-jsp-mode))
(add-hook 'web-mode-hook 'prettier-js-mode)

(use-package clojure-mode
  :ensure t
  :config
  (define-key clojure-mode-map (kbd "<M-return>") 'clerk-show))

(use-package cider
  :ensure t)


(use-package flycheck-clj-kondo
  :ensure t)

(dolist (checker '(clj-kondo-clj clj-kondo-cljs clj-kondo-cljc clj-kondo-edn))
  (setq flycheck-checkers (cons checker (delq checker flycheck-checkers))))

;; (add-hook 'clojure-mode-hook #'subword-mode)
;; (add-hook 'clojure-mode-hook #'paredit-mode)

(use-package hideshow-org
  :ensure t
  :init
  (add-hook 'clojure-mode-hook (lambda () (hs-org/minor-mode 1)))
  (add-hook 'php-mode-hook     (lambda () (hs-org/minor-mode 1)))
  (add-hook 'js2-mode-hook     (lambda () (hs-org/minor-mode 1)))
  (add-hook 'nix-mode-hook     (lambda () (hs-org/minor-mode 1))))

;; (use-package lsp-javascript
;;   :ensure t)

(add-to-list 'auto-mode-alist '("\\.flow\\'" . js2-mode))
;; (add-hook 'js2-mode-hook #'lsp-mode)
;; (add-hook 'js2-mode-hook 'lsp)
(add-hook 'js2-mode-hook 'flow-minor-enable-automatically)
(add-hook 'js2-mode-hook 'prettier-js-mode)
;; (add-hook 'js2-mode-hook      (lambda () (c-set-offset 'case-label '+)))

(setq org-confirm-babel-evaluate nil)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (python . t)
   (clojure . t)))

(setq user-emacs-directory "~/.dotfiles-old/programs/emacs/")

;; Automatically tangle our Emacs.org config file when we save it
(defun efs/org-babel-tangle-config ()
  (message (file-name-directory (buffer-file-name)))
  (when (string-equal (file-name-directory (buffer-file-name))
                      (expand-file-name user-emacs-directory))
    ;; Dynamic scoping to the rescue
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle))))

(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'efs/org-babel-tangle-config)))

(require 'org-tempo)
(add-to-list 'org-structure-template-alist '("sh" . "src shell"))
(add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
(add-to-list 'org-structure-template-alist '("bb" . "src babashka"))
(add-to-list 'org-structure-template-alist '("clj" . "src clojure"))

(defun efs/lsp-mode-setup ()
  (setq lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
  (lsp-headerline-breadcrumb-mode))


(use-package lsp-mode
  :ensure t
  :commands (lsp lsp-deferred)
  :hook (lsp-mode . efs/lsp-mode-setup)
  :init
  (setq lsp-keymap-prefix "C-c l")  ;; Or 'C-l', 's-l'
  :config
  (lsp-enable-which-key-integration t)
  :custom
  (lsp-lens-enable t)
  (lsp-signature-auto-activate t))

(use-package lsp-ui
  :ensure t
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-ui-doc-position 'bottom))

;; (use-package lsp-treemacs
;;   :ensure t
;;   :config
;;   (treemacs-space-between-root-nodes nil))

(defun clerk-show ()
  (interactive)
  (save-buffer)
  (let
      ((filename
        (buffer-file-name)))
    (when filename
      (cider-interactive-eval
       (concat "(nextjournal.clerk/show! \"" filename "\")")))))
