{ pkgs, ... }:
let
  emacsPackages = pkgs.emacs.pkgs.withPackages (epkgs:
    with epkgs.melpaPackages; [
      ag
      auto-complete
      cider
      clojure-mode
      color-theme
      company
      earthfile-mode
      flycheck-clj-kondo
      flycheck-clojure
      flycheck-gradle
      go-mode
      gradle-mode
      # helm-descbinds
      helm-make
      helm-projectile
      kubernetes
      lsp-mode
      magit
      nix-mode
      nvm
      nyan-mode
      # org
      org-ql
      org-roam
      paredit
      prettier
      projectile
      rainbow-delimiters
      scss-mode
      smartparens
      # use-package
      which-key
      # windmove
      windswap
    ]);
in {
  programs.emacs = {
    enable = true;
    extraConfig = ''
      (print "Hello World!")
      (server-start)
      (global-set-key (kbd "C-x C-k") 'kill-this-buffer)
      (nyan-mode)
      (rainbow-delimiters-mode-enable)
      (menu-bar-mode -1)
      (scroll-bar-mode -1)
      (setq package-archives
        '(("melpa" . "https://melpa.org/packages/")
          ("melpa-stable" . "https://stable.melpa.org/packages/")
          ;; ("org" . "https://orgmode.org/elpa/")
          ("elpa" . "https://elpa.gnu.org/packages/")))
      (windmove-default-keybindings)

      (setq org-log-done 'time)
      (setq org-roam-directory "~/org-roam/")
      (setq org-directory "~/org-roam/")
      (setq org-agenda-files '("~/org-roam" "~/org-roam/daily"))

      ;; (org-roam-setup)
      ;; (org-roam-capture-templates
      ;;   `(("d" "default" plain "%?"
      ;;    :if-new (file+head "%<%Y%m%d%H%M%S>-$\{slug}.org" "#+title: $\{title}\n")
      ;;    :unnarrowed t)))

      ;; (org-roam-db-autosync-mode)

      (smartparens-strict-mode +1)
      (rainbow-delimiters-mode +1)

      (setq org-roam-dailies-capture-templates
        `(("d" "default" entry "* %?\n:PROPERTIES:\n:CREATED: %T\n:END:"
           :if-new (file+head "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>\n"))))
      (setq org-roam-file-exclude-regexp
        (concat "^" (expand-file-name org-roam-directory) "logseq/.*"))
      (global-set-key (kbd "C-x n c")     'org-roam-dailies-capture-today)
      (global-set-key (kbd "C-x n f")     'org-roam-node-find)

      (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start t)

      (setq org-confirm-babel-evaluate nil)

      (org-babel-do-load-languages
        'org-babel-load-languages
        '((emacs-lisp . t)
          (python . t)
          (clojure . t)))

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

      (require 'projectile)
      (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
      (projectile-mode +1)
      (tool-bar-mode -1)

      (require 'helm-eshell)
      (global-set-key (kbd "M-x") 'helm-M-x)
      (global-set-key (kbd "C-x C-m") 'helm-M-x)
      (global-set-key (kbd "M-y") 'helm-show-kill-ring)
      (global-set-key (kbd "C-x b") 'helm-mini)
      (global-set-key (kbd "C-x C-b") 'helm-buffers-list)
      (global-set-key (kbd "C-x C-f") 'helm-find-files)
      (global-set-key (kbd "C-h f") 'helm-apropos)
      (global-set-key (kbd "C-h r") 'helm-info-emacs)
      (global-set-key (kbd "C-h C-l") 'helm-locate-library)
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
      (helm-mode 1)
      ;; enable Helm version of Projectile with replacment commands
      (helm-projectile-on)
    '';
    package = emacsPackages;
  };
}
