;;; custom --- Custom code
;;; Commentary:

;;; Code:
(setq prelude-guru nil)
(setq load-package-verbose t)

;; (setq package-archives
;;       '(("gnu" . "http://elpa.gnu.org/packages/")
;;         ("melpa" . "https://melpa.org/packages/")))

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

(add-to-list 'load-path "~/.emacs.d/personal/ag-and-a-half")
(require 'ag-and-a-half)
(defalias 'ag 'ag-and-a-half)

(use-package auto-complete
  :ensure t)

(use-package earthfile-mode :ensure t)
;; (use-package kubernetes :ensure t)
;; (use-package kubernetes-helm :ensure t)
;; (use-package helm :ensure t)

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

(use-package typescript-mode
  :ensure t)

(use-package scss-mode
  :ensure t)
(use-package docker :ensure t)

(use-package gradle-mode :ensure t)
(use-package flycheck-gradle
  :ensure t)
(use-package lsp-ui :ensure t)

(use-package company
  :ensure t)

(use-package company-flow
  :ensure t)

(use-package cider
  :ensure t)

(use-package go-mode
  :ensure t)

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
        `(("d" "default" entry "* %?\n:PROPERTIES:\n:CREATED: [%U]\n:END:"
           :if-new (file+head "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>\n")))))


(use-package magit-lfs
  :ensure t)

;; (add-hook 'java-mode-hook
;;           (lambda ()
;;             "Treat Java 1.5 @-style annotations as comments."
;;             (setq c-comment-start-regexp "(@|/(/|[*][*]?))")
;;             (modify-syntax-entry ?@ "< b" java-mode-syntax-table)))

(menu-bar-mode -1)
(scroll-bar-mode -1)

(dolist (checker '(clj-kondo-clj clj-kondo-cljs clj-kondo-cljc clj-kondo-edn))
  (setq flycheck-checkers (cons checker (delq checker flycheck-checkers))))

(add-to-list 'auto-mode-alist '("\\.jsp\\'" . crappy-jsp-mode))
(add-to-list 'auto-mode-alist '("\\.flow\\'" . js2-mode))

(use-package flycheck-clj-kondo
  :ensure t)

(use-package flycheck-flow
  :ensure t)

(use-package hideshow-org
  :ensure t
  :config
  (add-hook 'clojure-mode-hook (lambda () (hs-org/minor-mode 1)))
  (add-hook 'php-mode-hook     (lambda () (hs-org/minor-mode 1)))
  (add-hook 'js2-mode-hook     (lambda () (hs-org/minor-mode 1)))
  (add-hook 'nix-mode-hook     (lambda () (hs-org/minor-mode 1))))

;; (add-hook 'clojure-mode-hook #'subword-mode)
;; (add-hook 'clojure-mode-hook #'paredit-mode)

;; (add-hook 'c-mode-common-hook (lambda () (c-set-offset 'case-label '+)))
;; (add-hook 'js2-mode-hook      (lambda () (c-set-offset 'case-label '+)))
;; (add-to-list 'auto-mode-alist '("components\\/.*\\.js\\'" . rjsx-mode))

(add-hook 'js2-mode-hook 'flow-minor-enable-automatically)
(add-hook 'js2-mode-hook 'prettier-js-mode)
(add-hook 'web-mode-hook 'prettier-js-mode)

;; (setq tab-width 2)
;; (setq default-tab-width 2)

;; (require 'kubernetes)
;; (require 'kubernetes-helm)

;; see https://github.com/nextjournal/clerk
;; (require 'clojure-mode)
;; (require 'cider-format)

(use-package clojure-mode
  :ensure t
  :config
  (define-key clojure-mode-map (kbd "<M-return>") 'clerk-show))

(defun clerk-show ()
  (interactive)
  (save-buffer)
  (let
      ((filename
        (buffer-file-name)))
    (when filename
      (cider-interactive-eval
       (concat "(nextjournal.clerk/show! \"" filename "\")")))))


;; enable autosync
