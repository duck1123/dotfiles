;;; custom --- Custom code
;;; Commentary:

;;; Code:
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
 '(org-agenda-files
   '("~/Nextcloud/org-roam" "~/Nextcloud/org-roam/daily" "~/Nextcloud/org"))
 '(package-selected-packages
   '(org-roam kubernetes-helm go-mode docker gradle-mode flycheck-gradle adoc-mode nix-mode lsp-ui a async auto-complete company dash flycheck git-commit helm helm-core ht ivy js2-mode lsp-docker lsp-mode magit popup projectile reformatter swiper transient with-editor rjsx-mode css-mode helm-ack ack flow-minor-mode magit-lfs groovy-mode flymake-lua company-lua markdown-mode flycheck-flow elisp-slime-nav exec-path-from-shell imenu-anywhere hl-todo gitconfig-mode easy-kill anzu ace-window zop-to-char zenburn-theme yaml-mode which-key web-mode volatile-highlights undo-tree super-save smex smartrep smartparens rainbow-mode rainbow-delimiters protobuf-mode prettier-js paredit operate-on-number nyan-mode mustache-mode move-text memoize lua-mode hideshow-org helm-projectile helm-github-stars helm-descbinds helm-cider-history helm-cider gotest go-projectile gitignore-mode flycheck-clojure flycheck-clj-kondo flow-js2-mode expand-region editorconfig dockerfile-mode discover-my-major diminish diff-hl d-mode csv-mode crux crappy-jsp-mode counsel company-go company-flow browse-kill-ring avy ack-and-a-half ac-cider))
 '(projectile-globally-ignored-buffers '("target"))
 '(projectile-globally-ignored-directories
   '(".idea" ".eunit" ".git" ".hg" ".fslckout" ".bzr" "_darcs" ".tox" ".svn" ".stack-work" "vendor" "node_modules" "target" "build" "View"))
 '(safe-local-variable-values
   '((eval
      (lambda nil
        (defun cider-jack-in-wrapper-function
            (orig-fun &rest args)
          (if
              (and
               (boundp 'use-bb-dev)
               use-bb-dev)
              (message "Use `bb dev` to start the development server, then `cider-connect` to the port it specifies.")
            (apply orig-fun args)))
        (advice-add 'cider-jack-in :around #'cider-jack-in-wrapper-function)
        (when
            (not
             (featurep 'clerk))
          (let
              ((init-file-path
                (expand-file-name "clerk.el" default-directory)))
            (when
                (file-exists-p init-file-path)
              (load init-file-path)
              (require 'clerk))))))
     (use-bb-dev . t)
     (cider-clojure-cli-global-options . "-A:demo")
     (eval progn
           (define-clojure-indent
             (>defn 1)
             (a 1)
             (action 1)
             (behavior 1)
             (button 1)
             (describe 1)
             (div 1)
             (error-action 1)
             (footer 1)
             (form 1)
             (h2 1)
             (input 1)
             (it 1)
             (li 1)
             (nav 1)
             (ok-action 1)
             (p 1)
             (remote 1)
             (select 1)
             (specification 1)
             (table 1)
             (tbody 1)
             (thead 1)
             (tr 1)
             (ui-form 1)
             (ui-form-field 1)
             (ui-modal 1)
             (ui-modal-content 1)
             (ul 1)))
     (clojure-align-forms-automatically t)
     (eval progn
           (define-clojure-indent
             (>defn 1)
             (a 1)
             (action 1)
             (behavior 1)
             (button 1)
             (describe 1)
             (div 1)
             (error-action 1)
             (footer 1)
             (form 1)
             (h2 1)
             (input 1)
             (it 1)
             (li 1)
             (nav 1)
             (ok-action 1)
             (p 1)
             (remote 1)
             (select 1)
             (specification 1)
             (table 1)
             (tbody 1)
             (thead 1)
             (tr 1)
             (ui-form 1)
             (ui-form-field 1)
             (ui-modal-content 1)
             (ul 1)))))
 '(sh-basic-offset 2)
 '(smartparens-global-strict-mode t)
 '(whitespace-line-column 120))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )