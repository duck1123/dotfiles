;;; custom --- Custom code
;;; Commentary: This file is auto-generated. Edit the org file

;;; Code:

(setq prelude-guru nil)

(setq load-package-verbose t)

(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
        ("melpa-stable" . "https://stable.melpa.org/packages/")
        ;; ("org" . "https://orgmode.org/elpa/")
        ("elpa" . "https://elpa.gnu.org/packages/")))

(setq gc-cons-threshold (* 100 1024 1024))
(setq read-process-output-max (* 1024 1024))
(setq create-lockfiles nil)
;; (setq make-backup-files nil)

;; Change the user-emacs-directory to keep unwanted things out of ~/.emacs.d
(setq user-emacs-directory (expand-file-name "~/.cache/emacs/"))
(setq url-history-file (expand-file-name "url/history" user-emacs-directory))
(setq emacs-config-directory (expand-file-name "~/.emacs.d"))

(setq user-emacs-directory "~/dotfiles/programs/emacs/")

(prelude-require-packages
 '(crappy-jsp-mode
   editorconfig
   magit
   paredit))

(global-set-key (kbd "C-x C-k")     'kill-this-buffer)

;; Keep customization settings in a temporary file (thanks Ambrevar!)
(setq custom-file
      (if (boundp 'server-socket-dir)
          (expand-file-name "custom.el" server-socket-dir)
        (expand-file-name (format "emacs-custom-%s.el" (user-uid)) temporary-file-directory)))
(load custom-file t)

(set-default-coding-systems 'utf-8)

(server-start)

(use-package auto-complete
  :ensure t)

(use-package earthfile-mode :ensure t)

(use-package nushell-mode :ensure t)

(add-to-list 'load-path (file-name-concat emacs-config-directory "personal/ag-and-a-half"))
(require 'ag-and-a-half)
(defalias 'ag 'ag-and-a-half)

(use-package helm-github-stars
  :ensure t
  :commands (helm-github-stars)
  :bind (("S-C-g" . helm-github-stars))
  :config
  (setq helm-github-stars-username "duck1123"))

(defun my/helm-projectile-switch-project-to-magit ()
  "Use `magit-status` instead of the default action when switching projects."
  (interactive)
  (let ((projectile-switch-project-action #'magit-status))
    (helm-projectile-switch-project)))

(use-package helm-projectile
  :ensure t
  :bind (("C-c p p" . my/helm-projectile-switch-project-to-magit)))

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

(use-package company :ensure t)

(menu-bar-mode -1)
(scroll-bar-mode -1)

(use-package magit
  :ensure t
  :config
  (setq magit-commit-arguments '("--verbose" "--gpg-sign=80E3B47F0495EF7E")))

(use-package magit-lfs
  :ensure t
  :after magit)

(use-package typescript-mode
  :ensure t)

(use-package clojure-mode
  :ensure t
  :config
  (define-key clojure-mode-map (kbd "<M-return>") 'clerk-show))

(use-package cider :ensure t)

(use-package flycheck-clojure :ensure t)

(use-package flycheck-clj-kondo :ensure t)

(dolist (checker '(clj-kondo-clj clj-kondo-cljs clj-kondo-cljc clj-kondo-edn))
  (setq flycheck-checkers (cons checker (delq checker flycheck-checkers))))

(use-package org
  :ensure t
  :init
  (progn
    (setq org-log-done 'time)
    (setq org-directory "~/org-roam/"))
  :config
  (require 'org-protocol)
  ;; Capture templates for links to pages having [ and ]
  ;; characters in their page titles - notably ArXiv
  ;; From https://github.com/sprig/org-capture-extension
  (defun transform-square-brackets-to-round-ones(string-to-transform)
    "Transforms [ into ( and ] into ), other chars left unchanged."
    (concat
     (mapcar #'(lambda (c) (if (equal c ?[) ?\( (if (equal c ?]) ?\) c))) string-to-transform)))
  (setq org-capture-templates
        `(("p" "Protocol" entry (file+headline ,(concat org-directory "notes.org") "Inbox")
           "* %^{Title}\nSource: %u, %c\n #+BEGIN_QUOTE\n%i\n#+END_QUOTE\n\n\n%?")
          ("L" "Protocol Link" entry (file ,(concat org-directory "001 - browser-links.org"))
           "* [[%:link][%:description]] :link:\n:PROPERTIES:\n:CREATED: %T\n:END:\n\n%?"))))

(setq org-agenda-files
      '("~/org-roam" "~/org-roam/daily"))

(use-package org-roam
  :ensure t
  ;; :straight
  ;; (:host github :repo "org-roam/org-roam-ui" :branch "main" :files ("*.el" "out"))
  :after org

  :custom
  (org-roam-directory "~/org-roam/")
  (org-roam-capture-templates
   `(("d" "default" plain "%?"
      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
      :unnarrowed t)))

  :bind
  (("C-x n c" . org-roam-dailies-capture-today)
   ("C-x n C-d" . org-roam-dailies-goto-date)
   ("C-x n l" . org-roam-buffer-toggle)
   ("C-x n f" . org-roam-node-find)
   ("C-x n i" . org-roam-node-insert)
   ("C-x n C-t" . org-roam-dailies-goto-today))

  :config
  (org-roam-setup)
  (org-roam-db-autosync-mode)

  (setq org-roam-dailies-capture-templates
        `(("d" "default" entry "* %?\n:PROPERTIES:\n:CREATED: %T\n:END:"
           :if-new (file+head "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>\n"))))
  (setq org-roam-file-exclude-regexp
        (concat "^" (expand-file-name org-roam-directory) "logseq/.*"))
  (global-set-key (kbd "C-x n c")     'org-roam-dailies-capture-today)
  (global-set-key (kbd "C-x n f")     'org-roam-node-find))

(use-package org-ql
  :ensure t
  :after org)

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

(setq org-confirm-babel-evaluate nil)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (python . t)
   (clojure . t)))

;; Automatically tangle our Emacs.org config file when we save it
(defun efs/org-babel-tangle-config ()
  (message (file-name-directory (buffer-file-name)))
  (when (string-equal (file-name-directory (buffer-file-name))
                      (expand-file-name user-emacs-directory))
    ;; Dynamic scoping to the rescue
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle))))

(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'efs/org-babel-tangle-config)))

(defun efs/lsp-mode-setup ()
  (setq lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
  (lsp-headerline-breadcrumb-mode))

(use-package lsp-mode
  :ensure t
  :commands (lsp lsp-deferred)
  :hook
  ((js2-mode . lsp)
   (clojure-mode . 'lsp)
   (clojurescript-mode . 'lsp)
   (clojurec-mode . 'lsp)
   (lsp-mode . efs/lsp-mode-setup))
  :init
  (setq lsp-keymap-prefix "C-c l")  ;; Or 'C-l', 's-l'
  :custom
  (lsp-lens-enable t)
  (lsp-signature-auto-activate t))

(use-package helm-lsp
  :commands helm-lsp-workspace-symbol)

(use-package lsp-ui
  :ensure t
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-ui-doc-position 'bottom))

(use-package terraform-mode :ensure t)
