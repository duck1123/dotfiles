(use-package org
  :ensure t
  :init
  (setq org-confirm-babel-evaluate nil)
  (setq org-log-done 'time)
  :config
  (require 'org-protocol)
  (org-babel-do-load-languages
    'org-babel-load-languages
    '((emacs-lisp . t)
      (python . t)
      (clojure . t)))
  ;; Capture templates for links to pages having [ and ]
  ;; characters in their page titles - notably ArXiv
  ;; From https://github.com/sprig/org-capture-extension
  (defun transform-square-brackets-to-round-ones(string-to-transform)
    "Transforms [ into ( and ] into ), other chars left unchanged."
    (concat
      (mapcar #'(lambda (c) (if (equal c ?\[) ?\( (if (equal c ?\]) ?\) c))) string-to-transform)))
  (setq org-capture-templates
        `(("p" "Protocol" entry (file+headline ,(concat org-directory "notes.org") "Inbox")
           "* %^{Title}
Source: %u, %c
 #+BEGIN_QUOTE
%i
#+END_QUOTE


%?")
          ("L" "Protocol Link" entry (file ,(concat org-directory "001 - browser-links.org"))
           "* [[%:link][%:description]] :link:
:PROPERTIES:
:CREATED: %T
:END:

%?"))))

