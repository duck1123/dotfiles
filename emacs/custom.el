;;; custom --- Custom code
;;; Commentary:

;;; Code:
(setq prelude-guru nil)

(setq package-archives '(
                         ;; ("elpa" . "http://tromey.com/elpa/")
                         ("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")
                         ("org" . "http://orgmode.org/elpa/")
                         ))

(prelude-require-packages
 '(ac-cider
   ack-and-a-half
   apt-utils-ido
   auto-complete
   aws-ec2
   bts
   bts-github
   clojure-mode
   cider
   company
   ctags
   dash
   docker
   dockerfile-mode
   docker-tramp
   editorconfig
   edn
   ewmctrl
   feature-mode
   gh
   gh-md
   gist
   git-commit
   gitconfig-mode
   github-browse-file
   github-notifier
   gitignore-mode
   git-timemachine
   go-projectile
   helm
   helm-ack
   helm-ag
   helm-bundle-show
   helm-chrome
   helm-clojuredocs
   helm-company
   helm-helm-commands
   helm-descbinds
   helm-github-stars
   helm-package
   helm-projectile
   helm-rhythmbox
   hideshow-org
   idle-highlight-mode
   js2-mode
   json-mode
   kodi-remote
   less-css-mode
   log4j-mode
   magit
   markdown-mode
   nginx-mode
   nodejs-repl
   nyan-mode
   org
   org-gcal
   org-jira
   org-sync
   paredit
   popup
   rainbow-delimiters
   realgud
   restclient))

;; Emacs IRC client
(require 'prelude-erc)
(require 'prelude-ido) ;; Super charges Emacs completion for C-x C-f and more
(require 'prelude-helm) ;; Interface for narrowing and search
(require 'prelude-helm-everywhere) ;; Enable Helm everywhere
(require 'prelude-company)
;; (require 'prelude-key-chord) ;; Binds useful features to key combinations
;; (require 'prelude-mediawiki)
;; (require 'prelude-evil)

;;; Programming languages support
(require 'prelude-c)
(require 'prelude-clojure)
;; (require 'prelude-coffee)
;; (require 'prelude-common-lisp)
(require 'prelude-css)
(require 'prelude-emacs-lisp)
;; (require 'prelude-erlang)
;; (require 'prelude-elixir)
(require 'prelude-go)
;; (require 'prelude-haskell)
(require 'prelude-js)
;; (require 'prelude-latex)
(require 'prelude-lisp)
;; (require 'prelude-ocaml)
(require 'prelude-org) ;; Org-mode helps you keep TODO lists, notes and more
(require 'prelude-perl)
(require 'prelude-python)
(require 'prelude-ruby)
;; (require 'prelude-scala)
(require 'prelude-scheme)
(require 'prelude-shell)
;; (require 'prelude-scss)
(require 'prelude-web) ;; Emacs mode for web templates
(require 'prelude-xml)
(require 'prelude-yaml)

;; Emacs IRC client
(require 'prelude-erc)
(require 'prelude-ido) ;; Super charges Emacs completion for C-x C-f and more
(require 'prelude-helm) ;; Interface for narrowing and search
(require 'prelude-helm-everywhere) ;; Enable Helm everywhere
(require 'prelude-company)
;; (require 'prelude-key-chord) ;; Binds useful features to key combinations
;; (require 'prelude-mediawiki)
;; (require 'prelude-evil)

;;; Programming languages support
(require 'prelude-c)
(require 'prelude-clojure)
;; (require 'prelude-coffee)
;; (require 'prelude-common-lisp)
(require 'prelude-css)
(require 'prelude-emacs-lisp)
;; (require 'prelude-erlang)
;; (require 'prelude-elixir)
(require 'prelude-go)
;; (require 'prelude-haskell)
(require 'prelude-js)
;; (require 'prelude-latex)
(require 'prelude-lisp)
;; (require 'prelude-ocaml)
(require 'prelude-org) ;; Org-mode helps you keep TODO lists, notes and more
(require 'prelude-perl)
(require 'prelude-python)
(require 'prelude-ruby)
;; (require 'prelude-scala)
(require 'prelude-scheme)
(require 'prelude-shell)
;; (require 'prelude-scss)
(require 'prelude-web) ;; Emacs mode for web templates
(require 'prelude-xml)
(require 'prelude-yaml)

(global-set-key (kbd "<mouse-6>")   'next-buffer)
(global-set-key (kbd "<mouse-7>")   'prev-buffer)
(global-set-key (kbd "S-C-<left>")  'shrink-window-horizontally)
(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-C-<down>")  'shrink-window)
(global-set-key (kbd "S-C-<up>")    'enlarge-window)
(global-set-key (kbd "S-C-g")       'helm-github-stars)
(global-set-key (kbd "C-c a")       'org-agenda)
(global-set-key (kbd "C-c c")       'org-capture)
(global-set-key (kbd "C-c l")       'org-store-link)
(global-set-key (kbd "C-c C-SPC")   'hs-toggle-hiding)
(global-set-key (kbd "C-x C-c")     'save-buffers-kill-emacs)
(global-set-key (kbd "C-x C-k")     'kill-this-buffer)

(add-hook 'java-mode-hook
          (lambda ()
            "Treat Java 1.5 @-style annotations as comments."
            (setq c-comment-start-regexp "(@|/(/|[*][*]?))")
            (modify-syntax-entry ?@ "< b" java-mode-syntax-table)))

(rainbow-delimiters-mode-enable)
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
(add-to-list 'auto-mode-alist '("\\.t\\'" . perl-mode))

;; set this in all c-based programming modes
(require 'org-gcal)

(set-time-zone-rule "GMT")

;; (load-file "~/.dotfiles/emacs/private.el.gpg")

(require 'hideshow-org)

(add-hook 'clojure-mode-hook #'subword-mode)
(add-hook 'clojure-mode-hook #'paredit-mode)
(add-hook 'clojure-mode-hook  (lambda () (hs-org/minor-mode 1)))
(add-hook 'php-mode-hook      (lambda () (hs-org/minor-mode 1)))
(add-hook 'c-mode-common-hook (lambda () (c-set-offset 'case-label '+)))
(add-hook 'js2-mode-hook      (lambda () (c-set-offset 'case-label '+)))
(add-hook 'js2-mode-hook  (lambda () (hs-org/minor-mode 1)))
(add-to-list 'auto-mode-alist '("components\\/.*\\.js\\'" . rjsx-mode))

(setq default-tab-width 2)

;; (require 'bts)
;; (require 'bts-github)

;; ;; Key Binding
;; (global-unset-key (kbd "M-b"))
;; (global-set-key (kbd "M-b n")   'bts:ticket-new)
;; (global-set-key (kbd "M-b s")   'bts:summary-open)
;; (global-set-key (kbd "M-b p n") 'bts:project-new)
;; (global-set-key (kbd "M-b p u") 'bts:project-update)
;; (global-set-key (kbd "M-b p d") 'bts:project-remove)
;; (global-set-key (kbd "M-b p D") 'bts:project-remove-all)
;; (global-set-key (kbd "M-b q n") 'bts:query-new)
;; (global-set-key (kbd "M-b q u") 'bts:query-update)
;; (global-set-key (kbd "M-b q d") 'bts:query-remove)
;; (global-set-key (kbd "M-b q D") 'bts:query-remove-all)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-revert-check-vc-info t)
 '(aws-command "~/.local/bin/aws")
 '(browse-url-browser-function (quote browse-url-generic))
 '(browse-url-generic-program "google-chrome")
 '(c-basic-offset 2)
 '(cider-known-endpoints (quote (("local" "localhost" "7888"))))
 '(cider-prefer-local-resources t)
 '(cider-prompt-for-project-on-connect (quote when-needed))
 '(cider-repl-history-file "~/.cider-history")
 '(cider-repl-use-pretty-printing t)
 '(clojure-defun-indents
   (quote
    (describe describe-config it fact facts future-fact future-facts Given When Then)))
 '(company-auto-complete t)
 '(cperl-close-paren-offset -4)
 '(cperl-indent-parens-as-block t)
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
 '(magit-commit-arguments (quote ("--verbose")))
 '(magit-diff-arguments (quote ("--ignore-space-change" "--no-ext-diff" "--stat")))
 '(magit-fetch-arguments (quote ("--prune")))
 '(magit-log-arguments
   (quote
    ("--graph" "--color" "--decorate" "--simplify-by-decoration" "-n256")))
 '(malabar-repl-grooysh "~/.sdkman/candidates/groovy/2.4.5/bin/groovysh")
 '(mingus-mpd-host "dev.jiksnu.com")
 '(mingus-playlist-separator " | ")
 '(mingus-use-ido-mode-p t)
 '(mingus-use-mouse-p nil)
 '(nginx-indent-level 2)
 '(nxml-child-indent 2)
 '(nxml-sexp-element-flag t)
 '(nxml-slash-auto-complete-flag nil)
 '(org-agenda-files
   (quote
    ("~/Dropbox/org/inbox.org" "~/Dropbox/org/HelloWorld.org" "~/Dropbox/org/lmp.org" "~/Dropbox/org/medical.org" "~/Dropbox/org/Tasks.org")))
 '(org-capture-use-agenda-date t)
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
 '(web-mode-code-indent-offset 2)
 '(web-mode-css-indent-offset 2)
 '(web-mode-enable-element-content-fontification t)
 '(web-mode-enable-element-tag-fontification t)
 '(web-mode-enable-html-entities-fontification t)
 '(web-mode-markup-indent-offset 2)
 '(whitespace-line-column 120))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
