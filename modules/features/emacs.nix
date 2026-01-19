{ ... }:
{
  flake.types.generic.feature-options.emacs =
    { inputs, lib }:
    let
      inherit (inputs.self.types.generic) simpleFeature;
    in
    simpleFeature { inherit inputs lib; } "emacs feature";

  flake.modules.homeManager.emacs =
    {
      config,
      lib,
      pkgs,
      ...
    }:
    {
      config =
        let
          emacsPackages = pkgs.emacs.pkgs.withPackages (
            epkgs:
            let
              melpa = epkgs.melpaPackages;
              quickroam = epkgs.trivialBuild {
                pname = "quickroam";
                version = "git-2025-01-01";
                src = pkgs.fetchFromGitHub {
                  owner = "meedstrom";
                  repo = "quickroam";
                  rev = "master";
                  sha256 = "sha256-JzhpTZmYHtJ5MBicbP7JCp7b66ahX4piivAIf+8u8u0=";
                };
                packageRequires = [
                  melpa.org-roam
                  melpa.pcre2el
                ];
              };
            in
            with melpa;
            [
              ag
              # ac-cider
              auto-complete
              cider
              clojure-mode
              color-theme
              company
              earthfile-mode
              flycheck
              flycheck-clj-kondo
              flycheck-clojure
              flycheck-gradle
              general
              go-mode
              gradle-mode
              # helm-descbinds
              helm-make
              helm-projectile
              kubernetes
              lsp-mode
              lsp-ui
              magit
              nix-mode
              nushell-mode
              nushell-ts-mode
              nvm
              nyan-mode
              # org
              org-ql
              org-roam
              org-roam-ui
              quickroam
              paredit
              prettier
              projectile
              rainbow-delimiters
              reformatter
              scss-mode
              smartparens
              terraform-mode
              tree-sitter
              # use-package
              which-key
              # windmove
              windswap
              yaml-mode
            ]
          );
        in
        lib.mkIf config.host.features.emacs.enable {
          programs.ripgrep.enable = true;

          xdg.desktopEntries.emacsclient = {
            name = "Emacs Client";
            genericName = "Text Editor";
            exec = "emacsclient -c %F";
            icon = "emacs";
            mimeType = [
              "text/plain"
              "text/markdown"
              "text/x-markdown"
              "text/org"
              "text/x-org"
            ];
            terminal = false;
            categories = [
              "Utility"
              "TextEditor"
            ];
          };

          xdg.mimeApps.defaultApplications = {
            "text/plain" = [ "emacsclient.desktop" ];
            "text/markdown" = [ "emacsclient.desktop" ];
            "text/x-markdown" = [ "emacsclient.desktop" ];
            "text/org" = [ "emacsclient.desktop" ];
            "text/x-org" = [ "emacsclient.desktop" ];
          };

          programs.emacs = {
            enable = true;
            extraConfig = ''
              (setq load-package-verbose t)
              (setq gc-cons-threshold (* 100 1024 1024))
              (setq read-process-output-max (* 1024 1024))
              (setq create-lockfiles nil)
              (setq make-backup-files nil)

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

              (defun efs/lsp-mode-setup ()
                (setq lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
                (lsp-headerline-breadcrumb-mode))

              (windmove-default-keybindings)

              ;; (use-package ac-cider :ensure t)
              (use-package auto-complete :ensure t)
              (use-package cider :ensure t)
              (use-package clojure-mode :ensure t)

              (color-theme-initialize)
              (color-theme-hober)

              (use-package company :ensure t)
              (use-package earthfile-mode :ensure t)

              (use-package editorconfig
                :ensure t
                :config
                (editorconfig-mode 1))

              (use-package flycheck
                :ensure t)

              (use-package general
                :ensure t
                :config
                (general-define-key
                  "C-x C-k" 'kill-this-buffer
                  "C-x b"   'helm-mini))

              (use-package go-mode :ensure t)

              (use-package magit
                :ensure t
                :config
                (setq magit-commit-arguments '("--verbose" "--gpg-sign=80E3B47F0495EF7E")))

              (use-package nix-mode :ensure t)

              (use-package nyan-mode
                :ensure t
                :config
                (nyan-mode))

              (use-package org
                :ensure t
                :init
                (progn
                  (setq org-directory (expand-file-name "~/org-roam/")))
                  (setq org-agenda-files '("~/org-roam/" "~/org-roam/daily"))
                  (setq org-confirm-babel-evaluate nil)
                  (setq org-log-done 'time)
                :config
                (require 'org-protocol)
                (org-babel-do-load-languages
                  'org-babel-load-languages
                  '((emacs-lisp . t)
                    (python . t)
                    (clojure . t)))
                ;; Capture templates for links to pages having [ and ]
                ;; characters in their page titles - notably ArXiv
                ;; From https://github.com/sprig/org-capture-extension
                (defun transform-square-brackets-to-round-ones(string-to-transform)
                  "Transforms [ into ( and ] into ), other chars left unchanged."
                  (concat
                    (mapcar #'(lambda (c) (if (equal c ?\[) ?\( (if (equal c ?\]) ?\) c))) string-to-transform)))
                (setq org-capture-templates
                      `(("p" "Protocol" entry (file+headline ,(concat org-directory "notes.org") "Inbox")
                         "* %^{Title}
              Source: %u, %c
               #+BEGIN_QUOTE
              %i
              #+END_QUOTE


              %?")
                        ("L" "Protocol Link" entry (file ,(concat org-directory "001 - browser-links.org"))
                         "* [[%:link][%:description]] :link:
              :PROPERTIES:
              :CREATED: %T
              :END:

              %?"))))

              (use-package org-roam
                :ensure t
                :commands (org-roam-buffer-toggle org-roam-node-find org-roam-node-insert)

                :custom
                (org-roam-directory (expand-file-name "~/org-roam/"))
                (org-roam-capture-templates
                 `(("d" "default" plain "%?"
                    :if-new (file+head "%<%Y%m%d%H%M%S>-$\{slug}.org" "#+title: $\{title}
              ")
                    :unnarrowed t)))

                :bind
                (("C-x n c"   . org-roam-dailies-capture-today)
                 ("C-x n C-d" . org-roam-dailies-goto-date)
                 ("C-x n l"   . org-roam-buffer-toggle)
                 ("C-x n C-t" . org-roam-dailies-goto-today))

                :config
                (org-roam-db-autosync-enable)
                (org-roam-db-autosync-mode)
                (setq org-roam-dailies-capture-templates
                      `(("d" "default" entry "* %?
              :PROPERTIES:
              :CREATED: %T
              :END:"
                         :if-new (file+head "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>
              "))))
                (setq org-roam-file-exclude-regexp
                   (concat "^" (expand-file-name org-roam-directory) "logseq/.*")))

              (use-package quickroam
                :after org
                :config
                (quickroam-mode))

              (global-set-key (kbd "C-x n f")     'quickroam-find)
              (global-set-key (kbd "C-x n i")     'quickroam-insert)

              (use-package org-roam-ui
                :ensure t
                :after org-roam
                :config
                (setq org-roam-ui-sync-theme t
                      org-roam-ui-follow t
                      org-roam-ui-update-on-save t
                      org-roam-ui-open-on-start t))

              (use-package rainbow-delimiters
                :ensure t
                :init
                (rainbow-delimiters-mode-enable))

              ;; (use-package reformatter
              ;;   :ensure t
              ;;   :config
              ;;   (reformatter-define nix-format :program "nixpkgs-fmt")
              ;;   (add-hook 'nix-mode-hook #'nix-format-on-save-mode))

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

              (use-package projectile
                :ensure t
                :config
                (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

              (use-package terraform-mode :ensure t)

              (require 'helm-eshell)
              (global-set-key (kbd "M-x") 'helm-M-x)
              (global-set-key (kbd "C-x C-m") 'helm-M-x)
              (global-set-key (kbd "M-y") 'helm-show-kill-ring)
              (global-set-key (kbd "C-x C-b") 'helm-buffers-list)
              (global-set-key (kbd "C-x C-f") 'helm-find-files)
              (global-set-key (kbd "C-h f") 'helm-apropos)
              (global-set-key (kbd "C-h r") 'helm-info-emacs)
              (global-set-key (kbd "C-h C-l") 'helm-locate-library)
              (global-set-key (kbd "C-S-<right>") 'sp-forward-slurp-sexp)
              (global-set-key (kbd "C-M-s") 'sp-unwrap-sexp)
              ;; (define-key prelude-mode-map (kbd "C-c f") 'helm-recentf)
              (define-key minibuffer-local-map (kbd "C-c C-l") 'helm-minibuffer-history)
              (define-key isearch-mode-map (kbd "C-o") 'helm-occur-from-isearch)
              (define-key shell-mode-map (kbd "C-c C-l") 'helm-comint-input-ring)
              ;; use helm to list eshell history
              (add-hook 'eshell-mode-hook
                #'(lambda ()
                  (substitute-key-definition 'eshell-list-history 'helm-eshell-history eshell-mode-map)))
              (substitute-key-definition 'find-tag 'helm-etags-select global-map)
              ;; (helm-descbinds-mode)

              (projectile-mode +1)

              (helm-mode 1)
              ;; enable Helm version of Projectile with replacment commands
              (helm-projectile-on)
            '';
            package = emacsPackages;
          };
        };
    };
}
