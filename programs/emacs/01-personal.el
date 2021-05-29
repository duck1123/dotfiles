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
;;; 01-personal.el ends here

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
