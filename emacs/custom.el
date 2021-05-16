;;; custom --- Custom code
;;; Commentary:

;;; Code:
(setq prelude-guru nil)

;; (setq package-archives
;;       '(("gnu" . "http://elpa.gnu.org/packages/")
;;         ("melpa" . "https://melpa.org/packages/")))

(prelude-require-packages
 '(
   auto-complete
   clojure-mode
   cider
   company-flow
   crappy-jsp-mode
   editorconfig
   flycheck-clj-kondo
   flycheck-flow
   helm
   helm-github-stars
   helm-projectile
   hideshow-org
   magit
   nix-mode
   nyan-mode
   paredit
   prettier-js
   rainbow-delimiters
   ))

(require 'prelude-helm) ;; Interface for narrowing and search
(require 'prelude-helm-everywhere) ;; Enable Helm everywhere
(require 'prelude-company)
(require 'prelude-clojure)
(require 'prelude-css)
(require 'prelude-js)
(require 'prelude-shell)
(require 'prelude-web) ;; Emacs mode for web templates
(require 'prelude-xml)
(require 'prelude-yaml)

(global-set-key (kbd "S-C-g")       'helm-github-stars)
(global-set-key (kbd "C-x C-k")     'kill-this-buffer)

(add-to-list 'load-path "~/.emacs.d/personal/ag-and-a-half")
(require 'ag-and-a-half)
(defalias 'ag 'ag-and-a-half)

(require 'magit-lfs)

;; (add-hook 'java-mode-hook
;;           (lambda ()
;;             "Treat Java 1.5 @-style annotations as comments."
;;             (setq c-comment-start-regexp "(@|/(/|[*][*]?))")
;;             (modify-syntax-entry ?@ "< b" java-mode-syntax-table)))

;; (rainbow-delimiters-mode-enable)
(nyan-mode)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(require 'flycheck-clj-kondo)

(dolist (checker '(clj-kondo-clj clj-kondo-cljs clj-kondo-cljc clj-kondo-edn))
  (setq flycheck-checkers (cons checker (delq checker flycheck-checkers))))

(add-to-list 'auto-mode-alist '("\\.jsp\\'" . crappy-jsp-mode))
(add-to-list 'auto-mode-alist '("\\.flow\\'" . js2-mode))
(add-to-list 'auto-mode-alist '("\\Earthfile\\'" . dockerfile-mode))

(require 'hideshow-org)
;; (require 'flycheck-flow)

;; (add-hook 'clojure-mode-hook #'subword-mode)
;; (add-hook 'clojure-mode-hook #'paredit-mode)

(add-hook 'clojure-mode-hook (lambda () (hs-org/minor-mode 1)))
(add-hook 'php-mode-hook     (lambda () (hs-org/minor-mode 1)))
(add-hook 'js2-mode-hook     (lambda () (hs-org/minor-mode 1)))
(add-hook 'nix-mode-hook     (lambda () (hs-org/minor-mode 1)))


;; (add-hook 'c-mode-common-hook (lambda () (c-set-offset 'case-label '+)))
;; (add-hook 'js2-mode-hook      (lambda () (c-set-offset 'case-label '+)))
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
 '(company-auto-commit t)
 '(epg-gpg-program "gpg")
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
 '(projectile-globally-ignored-buffers '("target"))
 '(projectile-globally-ignored-directories
   '(".idea" ".eunit" ".git" ".hg" ".fslckout" ".bzr" "_darcs" ".tox" ".svn" ".stack-work" "vendor" "node_modules" "target" "build" "View"))
 '(safe-local-variable-values '((cider-default-cljs-repl . shadow)))
 '(sh-basic-offset 2)
 '(smartparens-global-strict-mode t)
 '(whitespace-line-column 120))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
