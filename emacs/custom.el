;;; custom --- Custom code
;;; Commentary:

;;; Code:
(setq prelude-guru nil)

(require 'cl)

(setq package-archives '(
                         ;; ("elpa" . "http://tromey.com/elpa/")
                         ("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")
                         ("org" . "http://orgmode.org/elpa/")
                         ))

(prelude-require-packages
 '(
   auto-complete
   clojure-mode
   cider
   company-flow
   crappy-jsp-mode
   editorconfig
   flycheck-flow
   helm
   helm-github-stars
   helm-projectile
   hideshow-org
   magit
   nyan-mode
   org
   paredit
   prettier-js
   rainbow-delimiters
   ))

;; Emacs IRC client
;; (require 'prelude-erc)
(require 'prelude-ido) ;; Super charges Emacs completion for C-x C-f and more
(require 'prelude-helm) ;; Interface for narrowing and search
(require 'prelude-helm-everywhere) ;; Enable Helm everywhere
(require 'prelude-company)
;; (require 'prelude-key-chord) ;; Binds useful features to key combinations
;; (require 'prelude-mediawiki)
;; (require 'prelude-evil)

;;; Programming languages support
;; (require 'prelude-c)
(require 'prelude-clojure)
;; (require 'prelude-coffee)
;; (require 'prelude-common-lisp)
(require 'prelude-css)
;; (require 'prelude-emacs-lisp)
;; (require 'prelude-erlang)
;; (require 'prelude-elixir)
;; (require 'prelude-go)
;; (require 'prelude-haskell)
(require 'prelude-js)
;; (require 'prelude-latex)
;; (require 'prelude-lisp)
;; (require 'prelude-ocaml)
(require 'prelude-org) ;; Org-mode helps you keep TODO lists, notes and more
;; (require 'prelude-perl)
;; (require 'prelude-python)
;; (require 'prelude-ruby)
;; (require 'prelude-scala)
;; (require 'prelude-scheme)
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

(dolist (checker '(clj-kondo-clj clj-kondo-cljs clj-kondo-cljc clj-kondo-edn))
  (setq flycheck-checkers (cons checker (delq checker flycheck-checkers))))

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
(add-to-list 'auto-mode-alist '("\\.flow\\'" . js2-mode))

;; set this in all c-based programming modes
;; (require 'org-gcal)

;; (set-time-zone-rule "GMT")

;; (load-file "~/.dotfiles/emacs/private.el.gpg")

(require 'hideshow-org)
(require 'flycheck-flow)

(add-hook 'clojure-mode-hook #'subword-mode)
(add-hook 'clojure-mode-hook #'paredit-mode)
(add-hook 'clojure-mode-hook  (lambda () (hs-org/minor-mode 1)))
(add-hook 'php-mode-hook      (lambda () (hs-org/minor-mode 1)))
(add-hook 'c-mode-common-hook (lambda () (c-set-offset 'case-label '+)))
(add-hook 'js2-mode-hook      (lambda () (c-set-offset 'case-label '+)))
(add-hook 'js2-mode-hook  (lambda () (hs-org/minor-mode 1)))
(add-to-list 'auto-mode-alist '("components\\/.*\\.js\\'" . rjsx-mode))

(add-hook 'js2-mode-hook 'flow-minor-enable-automatically)

(add-hook 'js2-mode-hook 'prettier-js-mode)
(add-hook 'web-mode-hook 'prettier-js-mode)

(setq tab-width 2)
(setq default-tab-width 2)

;; (require 'org-protocol)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
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
 '(global-org-gnome-minor-mode t)
 '(helm-github-stars-refetch-time 0.5)
 '(helm-github-stars-username "duck1123")
 '(js-indent-level 2)
 '(js-switch-indent-offset 2)
 '(js2-indent-switch-body t)
 '(js2-mode-indent-ignore-first-tab t)
 '(magit-commit-arguments (quote ("--verbose" "--gpg-sign=80E3B47F0495EF7E")))
 '(magit-diff-arguments (quote ("--ignore-space-change" "--no-ext-diff" "--stat")))
 '(magit-fetch-arguments (quote ("--prune")))
 '(magit-log-arguments
   (quote
    ("--graph" "--color" "--decorate" "--show-signature" "-n256")))
 '(magit-log-section-arguments (quote ("--graph" "--color" "--decorate" "-n256")))
 '(malabar-repl-grooysh "~/.sdkman/candidates/groovy/2.4.5/bin/groovysh")
 '(nxml-child-indent 2)
 '(nxml-sexp-element-flag t)
 '(nxml-slash-auto-complete-flag nil)
 '(org-agenda-files (quote ("~/Nextcloud/org/")))
 '(org-capture-use-agenda-date t)
 '(org-datetree-add-timestamp (quote active))
 '(org-gnome-integrate-with-calendar t)
 '(org-gnome-integrate-with-empathy nil)
 '(package-selected-packages
   (quote
    (ac-cider ack-and-a-half alert anaconda-mode anzu async auto-package-update avy beacon browse-kill-ring cider cider-spy cljdoc cljr-helm clojure-mode clojure-mode-extra-font-locking clojurescript-mode company company-anaconda company-go counsel counsel-spotify crux csv-mode dash diff-hl diminish discover-my-major docker dockerfile-mode easy-kill editorconfig epl expand-region f feature-mode flx-ido flycheck flycheck-clojure flycheck-color-mode-line flycheck-flow geiser gh gh-md gitignore-mode grizzl go-projectile god-mode gotest groovy-mode guru-mode helm helm-core helm-ack helm-bundle-show helm-clojuredocs helm-company helm-descbinds helm-github-stars helm-helm-commands helm-git helm-package helm-projectile helm-spotify-plus hideshow-org ht idle-highlight-mode ivy jade-mode js-auto-beautify js2-mode less-css-mode log4j-mode lua-mode magit magit-popup markdown-mode marshal move-text nodejs-repl npm-mode nyan-mode operate-on-number ov php-mode playerctl projectile puppet-mode pythonic rainbow-delimiters rainbow-mode realgud restclient restclient-test restclient-helm s smex smart-mode-line smartparens smartrep spotify swiper undo-tree volatile-highlights which-key yari yaml-mode zop-to-char)))
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
