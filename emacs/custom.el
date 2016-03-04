(setq prelude-guru nil)

(setq package-archives '(
                         ;; ("elpa" . "http://tromey.com/elpa/")
                         ("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")
                         ("org" . "http://orgmode.org/elpa/")
                         ))

(prelude-require-packages
 '(ack-and-a-half auth-password-store bts bts-github ctags helm-ack
                  helm-github-stars hideshow-org log4j-mode mingus nyan-mode
                  org-gcal org-sync paredit))

(global-set-key (kbd "<mouse-6>") 'next-buffer)
(global-set-key (kbd "<mouse-7>") 'prev-buffer)
(global-set-key (kbd "S-C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-C-<down>") 'shrink-window)
(global-set-key (kbd "S-C-<up>") 'enlarge-window)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c C-SPC") 'hs-toggle-hiding)
(global-set-key (kbd "S-C-g") 'helm-github-stars)
(global-set-key (kbd "C-x C-c") 'save-buffers-kill-emacs)
(global-set-key (kbd "C-x C-k") 'kill-this-buffer)

(add-hook 'java-mode-hook
          (lambda ()
            "Treat Java 1.5 @-style annotations as comments."
            (setq c-comment-start-regexp "(@|/(/|[*][*]?))")
            (modify-syntax-entry ?@ "< b" java-mode-syntax-table)))

(nyan-mode)
(scroll-bar-mode -1)

(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsp\\'" . crappy-jsp-mode))

;; set this in all c-based programming modes
(add-hook 'c-mode-common-hook
          (lambda ()
            (c-set-offset 'case-label '+)))

(require 'org-gcal)

(set-time-zone-rule "GMT")

;; (load-file "~/.dotfiles/emacs/private.el.gpg")

(require 'hideshow-org)

(add-hook 'php-mode-hook (lambda () (hs-org/minor-mode 1)))
(add-hook 'clojure-mode-hook #'subword-mode)
(add-hook 'clojure-mode-hook #'paredit-mode)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(browse-url-browser-function (quote browse-url-generic))
 '(browse-url-generic-program "google-chrome")
 '(cider-known-endpoints (quote (("dev.jiksnu.com" "7888"))))
 '(cider-prefer-local-resources t)
 '(cider-prompt-for-project-on-connect nil)
 '(cider-repl-history-file "~/.cider-history")
 '(clojure-defun-indents (quote (describe it fact facts future-fact future-facts)))
 '(company-auto-complete t)
 '(ede-auto-add-method (quote always))
 '(ede-project-directories (quote ("/home/daniel-renfer/projects/LMP")))
 '(helm-github-stars-refetch-time 0.5)
 '(helm-github-stars-username "duck1123")
 '(jdee-import-auto-collapse-imports t)
 '(jira-url "https://jira.helloworld.com/rpc/xmlrpc")
 '(jira-username "daniel.renfer@helloworld.com")
 '(jiralib-url "https://jira.helloworld.com/")
 '(js-indent-level 2)
 '(js-switch-indent-offset 2)
 '(js2-basic-offset 2)
 '(malabar-repl-grooysh "~/.sdkman/candidates/groovy/2.4.5/bin/groovysh")
 '(mingus-mpd-host "dev.jiksnu.com")
 '(mingus-playlist-separator " | ")
 '(mingus-use-ido-mode-p t)
 '(mingus-use-mouse-p nil)
 '(org-agenda-files
   (quote
    ("~/Dropbox/org/doing.org" "~/Dropbox/org/github.org" "~/Dropbox/org/inbox.org" "~/Dropbox/org/schedule.org" "~/Dropbox/org/hw-schedule.org" "~/.org-jira/MPD.org")))
 '(org-datetree-add-timestamp (quote active))
 '(org-default-notes-file "~/Dropbox/org/inbox.org")
 '(org-directory "~/Dropbox/org")
 '(org-doing-file "~/Dropbox/org/doing.org")
 '(org-jira-default-jql
   "project = MPD AND assignee in (EMPTY, currentUser()) and resolution = unresolved ORDER BY
  priority DESC, created ASC")
 '(projectile-create-missing-test-files t)
 '(projectile-enable-caching t)
 '(projectile-globally-ignored-buffers (quote ("target")))
 '(projectile-globally-ignored-directories
   (quote
    (".idea" ".eunit" ".git" ".hg" ".fslckout" ".bzr" "_darcs" ".tox" ".svn" ".stack-work" "vendor" "node_modules" "target" "build" "View")))
 '(projectile-tags-command "ctags-exuberant -Re -f \"%s\" %s")
 '(safe-local-variable-values (quote ((projectile-project-type (quote symphony)))))
 '(sh-indentation 2)
 '(smartparens-global-strict-mode t)
 '(whitespace-line-column 120))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
