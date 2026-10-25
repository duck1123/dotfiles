(require 'helm-eshell)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x C-m") 'helm-M-x)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
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

(helm-mode 1)
;; enable Helm version of Projectile with replacment commands
(helm-projectile-on)
