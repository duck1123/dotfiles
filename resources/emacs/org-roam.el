;; Org lives in the org-roam vault. This part runs before the org part so
;; these are set before `(use-package org)' loads org.
(setq org-directory (expand-file-name "~/org-roam/"))
(setq org-agenda-files
      (append (directory-files-recursively (expand-file-name "~/org-roam/daily/") "\\.org$")
              (directory-files-recursively (expand-file-name "~/org-roam/topics/") "\\.org$")))

(use-package org-roam
  :ensure t
  :commands (org-roam-buffer-toggle org-roam-node-find org-roam-node-insert)

  :custom
  (org-roam-directory (expand-file-name "~/org-roam/"))
  (org-roam-capture-templates
   `(("d" "default" plain "%?"
      :if-new (file+head "topics/%<%Y>/%<%m>/%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}
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
           :if-new (file+head "%<%Y>/%<%m>/%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>
"))))
  (setq org-roam-file-exclude-regexp
     (concat "^" (expand-file-name org-roam-directory) "logseq/.*")))

(use-package quickroam
  :after org
  :config
  (quickroam-mode))

(defun org-roam-capture--finalize-tags-insert ()
  "Append a link to the just-captured node onto the ROAM_TAGS
property of the entry `org-roam-tags-insert' was called from.
Mirrors `org-roam-capture--finalize-insert-link' but writes into
a property instead of inserting text at point."
  (when-let* ((mkr (org-roam-capture--get :call-location))
              (buf (marker-buffer mkr)))
    (with-current-buffer buf
      (org-with-point-at mkr
        (let* ((id (org-roam-capture--get :id))
               (description (org-roam-capture--get :link-description))
               (link (format "[[id:%s][%s]]" id description))
               (existing (org-entry-get (point) "ROAM_TAGS")))
          (org-entry-put (point) "ROAM_TAGS"
                         (if (and existing (not (string-empty-p existing)))
                             (concat existing " " link)
                           link)))))))

(defun org-roam-tags-insert ()
  "Prompt for a node via `quickroam-cache' and append a link to it
onto the current entry's ROAM_TAGS property, creating the property
(and its drawer) if needed. Skips nodes already present in the
value. Uses quickroam's cache rather than `org-roam-node-read' so
the prompt stays fast at this vault's size. If the entered title
doesn't match an existing node, falls into the normal org-roam
capture flow to create it, then appends the new node's link once
that capture is finished (C-c C-c)."
  (interactive)
  (require 'quickroam)
  (when (or (hash-table-empty-p quickroam-cache)
            (not quickroam-mode))
    (quickroam-reset))
  (let* ((title (completing-read "Tag: " quickroam-cache nil nil nil 'org-roam-node-history))
         (node (gethash title quickroam-cache)))
    (if node
        (let* ((id (plist-get node :id))
               (link (format "[[id:%s][%s]]" id title))
               (existing (org-entry-get (point) "ROAM_TAGS")))
          (if (and existing (string-match-p (regexp-quote id) existing))
              (message "Already tagged with %s" title)
            (org-entry-put (point) "ROAM_TAGS"
                           (if (and existing (not (string-empty-p existing)))
                               (concat existing " " link)
                             link))))
      (org-roam-capture-
       :node (org-roam-node-create :title title)
       :props (list :link-description title :finalize 'tags-insert)))))

(global-set-key (kbd "C-x n f")     'quickroam-find)
(global-set-key (kbd "C-x n i")     'quickroam-insert)
(global-set-key (kbd "C-x n t")     'org-roam-tags-insert)

(use-package org-roam-ui
  :ensure t
  :after org-roam
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start t))

