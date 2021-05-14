;;; custom --- Custom code
;;; Commentary:

;;; Code:
(setq prelude-guru nil)

;; (require 'cl)

(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")))

(prelude-require-packages
 '(
   ;; auto-complete
   clojure-mode
   cider
   ;; company-flow
   crappy-jsp-mode
   editorconfig
   flycheck-clj-kondo
   ;; flycheck-flow
   helm
   ;; helm-github-stars
   helm-projectile
   ;; hideshow-org
   magit
   nyan-mode
   paredit
   ;; prettier-js
   ;; rainbow-delimiters
   ))

;; Emacs IRC client
;; (require 'prelude-erc)
;; (require 'prelude-ido) ;; Super charges Emacs completion for C-x C-f and more
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
;; (require 'prelude-org) ;; Org-mode helps you keep TODO lists, notes and more
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

;; (global-set-key (kbd "<mouse-6>")   'next-buffer)
;; (global-set-key (kbd "<mouse-7>")   'prev-buffer)
;; (global-set-key (kbd "S-C-<left>")  'shrink-window-horizontally)
;; (global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
;; (global-set-key (kbd "S-C-<down>")  'shrink-window)
;; (global-set-key (kbd "S-C-<up>")    'enlarge-window)
(global-set-key (kbd "S-C-g")       'helm-github-stars)
;; (global-set-key (kbd "C-c a")       'org-agenda)
;; (global-set-key (kbd "C-c c")       'org-capture)
;; (global-set-key (kbd "C-c l")       'org-store-link)
;; (global-set-key (kbd "C-c C-SPC")   'hs-toggle-hiding)
;; (global-set-key (kbd "C-x C-c")     'save-buffers-kill-emacs)
(global-set-key (kbd "C-x C-k")     'kill-this-buffer)

(add-to-list 'load-path "~/.emacs.d/personal/ag-and-a-half")
(require 'ag-and-a-half)
(defalias 'ag 'ag-and-a-half)

(require 'magit-lfs)

(add-hook 'java-mode-hook
          (lambda ()
            "Treat Java 1.5 @-style annotations as comments."
            (setq c-comment-start-regexp "(@|/(/|[*][*]?))")
            (modify-syntax-entry ?@ "< b" java-mode-syntax-table)))

(rainbow-delimiters-mode-enable)
(nyan-mode)
(scroll-bar-mode -1)
(require 'flycheck-clj-kondo)

(dolist (checker '(clj-kondo-clj clj-kondo-cljs clj-kondo-cljc clj-kondo-edn))
  (setq flycheck-checkers (cons checker (delq checker flycheck-checkers))))

;; (add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.tpl\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsp\\'" . crappy-jsp-mode))
;; (add-to-list 'auto-mode-alist '("\\.t\\'" . perl-mode))
(add-to-list 'auto-mode-alist '("\\.flow\\'" . js2-mode))
(add-to-list 'auto-mode-alist '("\\Earthfile\\'" . dockerfile-mode))

;; (set-time-zone-rule "GMT")

;; (load-file "~/.dotfiles/emacs/private.el.gpg")

(require 'hideshow-org)
;; (require 'flycheck-flow)

;; (add-hook 'clojure-mode-hook #'subword-mode)
;; (add-hook 'clojure-mode-hook #'paredit-mode)
;; (add-hook 'clojure-mode-hook  (lambda () (hs-org/minor-mode 1)))
;; (add-hook 'php-mode-hook      (lambda () (hs-org/minor-mode 1)))
;; (add-hook 'c-mode-common-hook (lambda () (c-set-offset 'case-label '+)))
;; (add-hook 'js2-mode-hook      (lambda () (c-set-offset 'case-label '+)))
;; (add-hook 'js2-mode-hook  (lambda () (hs-org/minor-mode 1)))
;; (add-to-list 'auto-mode-alist '("components\\/.*\\.js\\'" . rjsx-mode))

(add-hook 'js2-mode-hook 'flow-minor-enable-automatically)
;; (add-hook 'js2-mode-hook 'prettier-js-mode)
;; (add-hook 'web-mode-hook 'prettier-js-mode)

;; (setq tab-width 2)
;; (setq default-tab-width 2)

;; (require 'kubernetes)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(browse-url-browser-function 'browse-url-generic)
 '(browse-url-generic-program "google-chrome")
 '(c-basic-offset 2)
 '(cider-known-endpoints '(("local" "localhost" "7000")))
 '(cider-prefer-local-resources t)
 '(cider-prompt-for-project-on-connect 'when-needed)
 '(cider-repl-pop-to-buffer-on-connect 'display-only)
 '(cider-repl-require-ns-on-set t)
 '(cider-repl-use-pretty-printing t)
 '(clojure-align-cond-forms '("condp" "cond" "cond->" "cond->>" "case" "are"))
 '(clojure-defun-indents
   '(describe describe-config it fact facts future-fact future-facts Given When Then context GET POST DELETE fn-traced))
 '(company-auto-commit t)
 '(epg-gpg-program "gpg")
 '(flycheck-checkers
   '(clj-kondo-edn clj-kondo-cljc clj-kondo-cljs clj-kondo-clj ada-gnat asciidoctor asciidoc awk-gawk bazel-buildifier c/c++-clang c/c++-gcc c/c++-cppcheck cfengine chef-foodcritic coffee coffee-coffeelint coq css-csslint css-stylelint cuda-nvcc cwl d-dmd dockerfile-hadolint elixir-credo emacs-lisp emacs-lisp-checkdoc ember-template erlang-rebar3 erlang eruby-erubis eruby-ruumba fortran-gfortran go-gofmt go-golint go-vet go-build go-test go-errcheck go-unconvert go-staticcheck groovy haml handlebars haskell-stack-ghc haskell-ghc haskell-hlint html-tidy javascript-jshint javascript-standard json-jsonlint json-python-json json-jq jsonnet less less-stylelint llvm-llc lua-luacheck lua markdown-markdownlint-cli markdown-mdl nix nix-linter opam perl perl-perlcritic php php-phpmd php-phpcs processing proselint protobuf-protoc protobuf-prototool pug puppet-parser puppet-lint python-flake8 python-pylint python-pycompile python-pyright python-mypy r-lintr racket rpm-rpmlint rst-sphinx rst ruby-rubocop ruby-standard ruby-reek ruby-rubylint ruby ruby-jruby rust-cargo rust rust-clippy scala scala-scalastyle scheme-chicken scss-lint scss-stylelint sass/scss-sass-lint sass scss sh-bash sh-posix-dash sh-posix-bash sh-zsh sh-shellcheck slim slim-lint sql-sqlint systemd-analyze tcl-nagelfar terraform terraform-tflint tex-chktex tex-lacheck texinfo textlint typescript-tslint verilog-verilator vhdl-ghdl xml-xmlstarlet xml-xmllint yaml-jsyaml yaml-ruby yaml-yamllint))
 '(flycheck-disabled-checkers '(javascript-jshint))
 '(helm-github-stars-refetch-time 0.5)
 '(helm-github-stars-username "duck1123")
 '(js-indent-level 4)
 '(js-switch-indent-offset 2)
 '(js2-indent-switch-body t)
 '(js2-mode-assume-strict t)
 '(js2-mode-indent-ignore-first-tab t)
 '(magit-commit-arguments '("--verbose" "--gpg-sign=80E3B47F0495EF7E"))
 '(magit-diff-arguments '("--ignore-space-change" "--no-ext-diff" "--stat"))
 '(magit-fetch-arguments '("--prune"))
 '(magit-log-arguments
   '("--graph" "--color" "--decorate" "--show-signature" "-n256"))
 '(magit-log-section-arguments '("--graph" "--color" "--decorate" "-n256"))
 '(nxml-child-indent 2)
 '(nxml-sexp-element-flag t)
 '(nxml-slash-auto-complete-flag nil)
 '(package-selected-packages
   '(nix-mode lsp-ui a async auto-complete company dash flycheck git-commit helm helm-core ht ivy js2-mode lsp-docker lsp-mode magit popup projectile reformatter swiper transient with-editor rjsx-mode css-mode helm-ack ack flow-minor-mode magit-lfs groovy-mode flymake-lua company-lua markdown-mode flycheck-flow elisp-slime-nav exec-path-from-shell imenu-anywhere hl-todo gitconfig-mode easy-kill anzu ace-window zop-to-char zenburn-theme yaml-mode which-key web-mode volatile-highlights undo-tree super-save smex smartrep smartparens rainbow-mode rainbow-delimiters protobuf-mode prettier-js paredit operate-on-number nyan-mode mustache-mode move-text memoize lua-mode hideshow-org helm-projectile helm-github-stars helm-descbinds helm-cider-history helm-cider gotest go-projectile gitignore-mode flycheck-clojure flycheck-clj-kondo flow-js2-mode expand-region editorconfig dockerfile-mode discover-my-major diminish diff-hl d-mode csv-mode crux crappy-jsp-mode counsel company-go company-flow browse-kill-ring avy ack-and-a-half ac-cider))
 '(projectile-enable-caching t)
 '(projectile-globally-ignored-buffers '("target"))
 '(projectile-globally-ignored-directories
   '(".idea" ".eunit" ".git" ".hg" ".fslckout" ".bzr" "_darcs" ".tox" ".svn" ".stack-work" "vendor" "node_modules" "target" "build" "View"))
 '(projectile-tags-command "ctags-exuberant -Re -f \"%s\" %s")
 '(safe-local-variable-values
   '((eval progn
           (define-clojure-indent
             (>defn 1)
             (a 1)
             (behavior 1)
             (button 1)
             (div 1)
             (footer 1)
             (form 1)
             (h2 1)
             (input 1)
             (nav 1)
             (select 1)
             (specification 1)
             (table 1)
             (tbody 1)
             (thead 1)
             (tr 1)
             (ui-form 1)
             (ui-form-field 1)
             (ui-modal-content 1)))
     (clojure-align-forms-automatically t)
     (eval progn
           (define-clojure-indent
             (>defn 1)
             (a 1)
             (behavior 1)
             (button 1)
             (div 1)
             (footer 1)
             (h2 1)
             (input 1)
             (nav 1)
             (select 1)
             (specification 1)
             (table 1)
             (tbody 1)
             (thead 1)
             (tr 1)
             (ui-form 1)
             (ui-form-field 1)
             (ui-modal-content 1)))
     (cider-figwheel-main-default-options . "dev")
     (cider-default-cljs-repl . figwheel-main)
     (cider-shadow-cljs-default-options . "app")
     (cider-default-cljs-repl . shadow)
     (projectile-project-type 'symphony)))
 '(sh-basic-offset 2)
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
