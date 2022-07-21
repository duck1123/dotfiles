;;; ag-and-a-half.el --- Yet another front-end for ag
;;
;; Copyright (C) 2013 Jacob Helwig <jacob@technosorcery.net>
;; Alexey Lebedeff <binarin@binarin.ru>
;; Andrew Pennebaker <andrew.pennebaker@gmail.com>
;; Andrew Stine <stine.drew@gmail.com>
;; Derek Chen-Becker <derek@precog.com>
;; Gleb Peregud <gleber.p@gmail.com>
;; Kim van Wyk <vanwykk@gmail.com>
;; Lars Andersen <expez@expez.com>
;; Ronaldo M. Ferraz <ronaldoferraz@gmail.com>
;; Ryan Thompson <rct@thompsonclan.org>
;;
;; Author: Jacob Helwig <jacob+ack@technosorcery.net>
;; Homepage: http://technosorcery.net
;; Version: 1.2.0
;; URL: https://github.com/jhelwig/ag-and-a-half
;;
;; This file is NOT part of GNU Emacs.
;;
;; Permission is hereby granted, free of charge, to any person obtaining a copy of
;; this software and associated documentation files (the "Software"), to deal in
;; the Software without restriction, including without limitation the rights to
;; use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies
;; of the Software, and to permit persons to whom the Software is furnished to do
;; so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be included in all
;; copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.
;;
;;; Commentary:
;;
;; ag-and-a-half.el provides a simple compilation mode for the silver searcher
;; (https://github.com/ggreer/the_silver_searcher).
;;
;; Add the following to your .emacs:
;;
;;     (add-to-list 'load-path "/path/to/ag-and-a-half")
;;     (require 'ag-and-a-half)
;;     (defalias 'ag 'ag-and-a-half)
;;     (defalias 'ag-same 'ag-and-a-half-same)
;;     (defalias 'ag-find-file 'ag-and-a-half-find-file)
;;     (defalias 'ag-find-file-same 'ag-and-a-half-find-file-same)
;;
;; Run `ag' to search for all files and `ag-same' to search for
;; files of the same type as the current buffer.
;;
;; `next-error' and `previous-error' can be used to jump to the
;; matches.
;;
;; `ag-find-file' and `ag-find-same-file' use ag to list the files
;; in the current project.  It's a convenient, though slow, way of
;; finding files.
;;

;;; Code:

(eval-when-compile (require 'cl))
(require 'compile)
(require 'grep)
(require 'thingatpt)

(add-to-list 'debug-ignored-errors
             "^Moved \\(back before fir\\|past la\\)st match$")
(add-to-list 'debug-ignored-errors "^File .* not found$")

(define-compilation-mode ag-and-a-half-mode "Ag"
  "Ag results compilation mode."
  (set (make-local-variable 'truncate-lines) t)
  (set (make-local-variable 'compilation-disable-input) t)
  (let ((smbl  'compilation-ag-nogroup)
        (pttrn '("^\\([^:\n]+?\\):\\([0-9]+\\):\\([0-9]+\\):" 1 2 3)))
    (set (make-local-variable 'compilation-error-regexp-alist) (list smbl))
    (set (make-local-variable 'compilation-error-regexp-alist-alist) (list (cons smbl pttrn))))
  (set (make-local-variable 'compilation-process-setup-function) 'ag-and-a-half-mode-setup)
  (set (make-local-variable 'compilation-error-face) grep-hit-face))

(defgroup ag-and-a-half nil "Yet another front end for ag."
  :group 'tools
  :group 'matching)

(defcustom ag-and-a-half-executable (executable-find "ag")
  "*The location of the ag executable."
  :group 'ag-and-a-half
  :type 'file)

(defcustom ag-and-a-half-buffer-name "*ag-and-a-half*"
  "*The name of the ag-and-a-half buffer."
  :group 'ag-and-a-half
  :type 'string)

(defun ag-buffer-name (mode) ag-and-a-half-buffer-name)

(defcustom ag-and-a-half-arguments nil
  "*Extra arguments to pass to ag."
  :group 'ag-and-a-half
  :type '(repeat (string)))

(defcustom ag-and-a-half-mode-type-alist nil
  "*File type(s) to search per major mode.  (ag-and-a-half-same)
This overrides values in `ag-and-a-half-mode-type-default-alist'.
The car in each list element is a major mode, and the rest
is a list of strings passed to the --type flag of ag when running
`ag-and-a-half-same'."
  :group 'ag-and-a-half
  :type '(repeat (cons (symbol :tag "Major mode")
                       (repeat (string :tag "ag --type")))))

(defcustom ag-and-a-half-mode-extension-alist nil
  "*File extensions to search per major mode.  (ag-and-a-half-same)
This overrides values in `ag-and-a-half-mode-extension-default-alist'.
The car in each list element is a major mode, and the rest
is a list of file extensions to be searched in addition to
the type defined in `ag-and-a-half-mode-type-alist' when
running `ag-and-a-half-same'."
  :group 'ag-and-a-half
  :type '(repeat (cons (symbol :tag "Major mode")
                       (repeat :tag "File extensions" (string)))))

(defcustom ag-and-a-half-ignore-case 'smart
  "*Whether or not to ignore case when searching.
The special value 'smart enables the ag option \"smart-case\"."
  :group 'ag-and-a-half
  :type '(choice (const :tag "Case sensitive" nil)
                 (const :tag "Smart case" 'smart)
                 (const :tag "Case insensitive" t)))

(defcustom ag-and-a-half-regexp-search t
  "*Default to regular expression searching.
Giving a prefix argument to `ag-and-a-half' toggles this option."
  :group 'ag-and-a-half
  :type '(choice (const :tag "Literal searching" nil)
                 (const :tag "Regular expression searching" t)))

(defcustom ag-and-a-half-root-directory-functions '(ag-and-a-half-guess-project-root)
  "*List of functions used to find the base directory to ag from.
These functions are called until one returns a directory.  If successful,
`ag-and-a-half' is run from that directory instead of from `default-directory'.
The directory is verified by the user depending on `ag-and-a-half-prompt-for-directory'."
  :group 'ag-and-a-half
  :type '(repeat function))

(defcustom ag-and-a-half-project-root-file-patterns
  '(".project\\'"
    ".xcodeproj\\'"
    ".sln\\'"
    "\\`Project.ede\\'"
    "\\`.git\\'"
    "\\`.bzr\\'"
    "\\`_darcs\\'"
    "\\`.hg\\'")
  "*List of file patterns for the project root (used by `ag-and-a-half-guess-project-root').
Each element is a regular expression.  If a file matching any element is
found in a directory, then that directory is assumed to be the project
root by `ag-and-a-half-guess-project-root'."
  :group 'ag-and-a-half
  :type '(repeat (string :tag "Regular expression")))

(defcustom ag-and-a-half-prompt-for-directory 'unless-guessed
  "*Prompt for directory in which to run ag.
If this is 'unless-guessed, then the value determined by
`ag-and-a-half-root-directory-functions' is used without
confirmation.  If it is nil, then the directory is never
confirmed.  If t, then always prompt for the directory to use."
  :group 'ag-and-a-half
  :type '(choice (const :tag "Don't prompt" nil)
                 (const :tag "Don't prompt when guessed" unless-guessed)
                 (const :tag "Always prompt" t)))

(defcustom ag-and-a-half-use-ido nil
  "Whether or not ag-and-a-half should use ido to provide
  completion suggestions when prompting for directory.")

;;; Default setting lists ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst ag-and-a-half-mode-type-default-alist
  '((actionscript-mode "actionscript")
    (LaTeX-mode "tex")
    (TeX-mode "tex")
    (asm-mode "asm")
    (batch-file-mode "batch")
    (c++-mode "cpp")
    (c-mode "cc")
    (cfmx-mode "cfmx")
    (cperl-mode "perl")
    (csharp-mode "csharp")
    (css-mode "css")
    (emacs-lisp-mode "elisp")
    (erlang-mode "erlang")
    (espresso-mode "java")
    (fortran-mode "fortran")
    (go-mode "go")
    (haskell-mode "haskell")
    (hexl-mode "binary")
    (html-mode "html")
    (java-mode "java")
    (javascript-mode "js")
    (jde-mode "java")
    (js2-mode "js")
    (jsp-mode "jsp")
    (latex-mode "tex")
    (lisp-mode "lisp")
    (lua-mode "lua")
    (makefile-mode "make")
    (mason-mode "mason")
    (nxml-mode "xml")
    (objc-mode "objc" "objcpp")
    (ocaml-mode "ocaml")
    (parrot-mode "parrot")
    (perl-mode "perl")
    (php-mode "php")
    (plone-mode "plone")
    (python-mode "python")
    (ruby-mode "ruby")
    (enh-ruby-mode "ruby")
    (scala-mode "scala")
    (scheme-mode "scheme")
    (shell-script-mode "shell")
    (skipped-mode "skipped")
    (smalltalk-mode "smalltalk")
    (sql-mode "sql")
    (tcl-mode "tcl")
    (tex-mode "tex")
    (tt-mode "tt")
    (vb-mode "vb")
    (vim-mode "vim")
    (xml-mode "xml")
    (yaml-mode "yaml"))
  "Default values for `ag-and-a-half-mode-type-alist'.")

(defconst ag-and-a-half-mode-extension-default-alist
  '((d-mode "d"))
  "Default values for `ag-and-a-half-mode-extension-alist'.")

(defun ag-and-a-half-create-type (extensions)
  (list "--type-set"
        (concat "ag-and-a-half-custom-type=" (mapconcat 'identity extensions ","))
        "--type" "ag-and-a-half-custom-type"))

(defun ag-and-a-half-type-for-major-mode (mode)
  "Return the --type and --type-set arguments to use with ag for major mode MODE."
  (let ((types (cdr (or (assoc mode ag-and-a-half-mode-type-alist)
                        (assoc mode ag-and-a-half-mode-type-default-alist))))
        (ext (cdr (or (assoc mode ag-and-a-half-mode-extension-alist)
                      (assoc mode ag-and-a-half-mode-extension-default-alist))))
        result)
    (dolist (type types)
      (push type result)
      (push "--type" result))
    (if ext
        (if types
            `("--type-add" ,(concat (car types)
                                    "=" (mapconcat 'identity ext ","))
              . ,result)
          (ag-and-a-half-create-type ext))
      result)))

;;; Project root ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ag-and-a-half-guess-project-root ()
  "Guess the project root directory.
This is intended to be used in `ag-and-a-half-root-directory-functions'."
  (catch 'root
    (let ((dir (expand-file-name (if buffer-file-name
                                     (file-name-directory buffer-file-name)
                                   default-directory)))
          (pattern (mapconcat 'identity ag-and-a-half-project-root-file-patterns "\\|")))
      (while (not (equal dir "/"))
        (when (directory-files dir nil pattern t)
          (throw 'root dir))
        (setq dir (file-name-directory (directory-file-name dir)))))))

;;; Commands ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar ag-and-a-half-directory-history nil
  "Directories recently searched with `ag-and-a-half'.")
(defvar ag-and-a-half-literal-history nil
  "Strings recently searched for with `ag-and-a-half'.")
(defvar ag-and-a-half-regexp-history nil
  "Regular expressions recently searched for with `ag-and-a-half'.")

(defun ag-and-a-half-initial-contents-for-read ()
  (when (ag-and-a-half-use-region-p)
    (buffer-substring-no-properties (region-beginning) (region-end))))

(defun ag-and-a-half-default-for-read ()
  (unless (ag-and-a-half-use-region-p)
    (thing-at-point 'symbol)))

(defun ag-and-a-half-use-region-p ()
  (or (and (fboundp 'use-region-p) (use-region-p))
      (and transient-mark-mode mark-active
           (> (region-end) (region-beginning)))))

(defsubst ag-and-a-half-read (regexp)
  (let* ((default (ag-and-a-half-default-for-read))
         (type (if regexp "pattern" "literal search"))
         (history-var )
         (prompt  (if default
                      (format "ag %s (default %s): " type default)
                    (format "ag %s: " type))))
    (read-string prompt
                 (ag-and-a-half-initial-contents-for-read)
                 (if regexp 'ag-regexp-history 'ag-literal-history)
                 default)))

(defun ag-and-a-half-read-dir ()
  (let ((dir (run-hook-with-args-until-success 'ag-and-a-half-root-directory-functions)))
    (if ag-and-a-half-prompt-for-directory
        (if (and dir (eq ag-and-a-half-prompt-for-directory 'unless-guessed))
            dir
          (if ag-and-a-half-use-ido
              (ido-read-directory-name "Directory: " dir dir t)
            (read-directory-name "Directory: " dir dir t)))
      (or dir
          (and buffer-file-name (file-name-directory buffer-file-name))
          default-directory))))

(defsubst ag-and-a-half-xor (a b)
  (if a (not b) b))

(defun ag-and-a-half-interactive ()
  "Return the (interactive) arguments for `ag-and-a-half' and `ag-and-a-half-same'."
  (let ((regexp (ag-and-a-half-xor current-prefix-arg ag-and-a-half-regexp-search)))
    (list (ag-and-a-half-read regexp)
          regexp
          (ag-and-a-half-read-dir))))

(defun ag-and-a-half-type ()
  (or (ag-and-a-half-type-for-major-mode major-mode)
      (when buffer-file-name
        (ag-and-a-half-create-type (list (file-name-extension buffer-file-name))))))

(defun ag-and-a-half-option (name enabled)
  (format "--%s%s" (if enabled "" "no") name))

(defun ag-and-a-half-arguments-from-options (regexp)
  (let ((arguments (list "--nocolor" "--nogroup" "--column"
                         (ag-and-a-half-option "smart-case" (eq ag-and-a-half-ignore-case 'smart))
                         )))
    (unless ag-and-a-half-ignore-case
      (push "-i" arguments))
    (unless regexp
      (push "--literal" arguments))
    arguments))

(defun ag-and-a-half-string-replace (from to string &optional re)
  "Replace all occurrences of FROM with TO in STRING.
All arguments are strings.  When optional fourth argument (RE) is
non-nil, treat FROM as a regular expression."
  (let ((pos 0)
        (res "")
        (from (if re from (regexp-quote from))))
    (while (< pos (length string))
      (if (setq beg (string-match from string pos))
          (progn
            (setq res (concat res
                              (substring string pos (match-beginning 0))
                              to))
            (setq pos (match-end 0)))
        (progn
          (setq res (concat res (substring string pos (length string))))
          (setq pos (length string)))))
    res))

(defun ag-and-a-half-run (directory regexp pattern &rest arguments)
  "Run ag in DIRECTORY with ARGUMENTS."
  (let ((default-directory (if directory
                               (file-name-as-directory (expand-file-name directory))
                             default-directory)))
    (setq arguments (append ag-and-a-half-arguments
                            (ag-and-a-half-arguments-from-options regexp)
                            arguments
                            (list "--")
                            (list (shell-quote-argument pattern))
                            (when (eq system-type 'windows-nt)
                              (list (concat " < " null-device)))
                            ))
    (make-local-variable 'compilation-buffer-name-function)
    (let (compilation-buffer-name-function)
      (setq compilation-buffer-name-function 'ag-buffer-name)
      (compilation-start (mapconcat 'identity (nconc (list ag-and-a-half-executable) arguments) " ")
                         'ag-and-a-half-mode))))

(defun ag-and-a-half-read-file (prompt choices)
  (if ido-mode
      (ido-completing-read prompt choices nil t)
    (require 'iswitchb)
    (with-no-warnings
      (let ((iswitchb-make-buflist-hook
             (lambda () (setq iswitchb-temp-buflist choices))))
        (iswitchb-read-buffer prompt nil t)))))

(defun ag-and-a-half-list-files (directory &rest arguments)
  (with-temp-buffer
    (let ((default-directory directory))
      (when (eq 0 (apply 'call-process ag-and-a-half-executable nil t nil "-f" "--print0"
                         arguments))
        (goto-char (point-min))
        (let ((beg (point-min))
              files)
          (while (re-search-forward "\0" nil t)
            (push (buffer-substring beg (match-beginning 0)) files)
            (setq beg (match-end 0)))
          files)))))

(defun ag-and-a-half-version-string ()
  "Return the ag version string."
  (with-temp-buffer
    (call-process ag-and-a-half-executable nil t nil "--version")
    (goto-char (point-min))
    (re-search-forward " +")
    (buffer-substring (point) (point-at-eol))))

(defun ag-and-a-half-mode-setup ()
  "Setup compilation variables and buffer for `ag-and-a-half'.
Set up `compilation-exit-message-function'."
  (set (make-local-variable 'compilation-exit-message-function)
       (lambda (status code msg)
         (if (eq status 'exit)
             (cond ((and (zerop code) (buffer-modified-p))
                    '("finished (matches found)\n" . "matched"))
                   ((not (buffer-modified-p))
                    '("finished with no matches found\n" . "no match"))
                   (t
                    (cons msg code)))
           (cons msg code)))))

;;; Public interface ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;###autoload
(defun ag-and-a-half (pattern &optional regexp directory)
  "Run ag.
PATTERN is interpreted as a regular expression, iff REGEXP is
non-nil.  If called interactively, the value of REGEXP is
determined by `ag-and-a-half-regexp-search'.  A prefix arg
toggles the behavior.  DIRECTORY is the root directory.  If
called interactively, it is determined by
`ag-and-a-half-project-root-file-patterns'.  The user is only
prompted, if `ag-and-a-half-prompt-for-directory' is set."
  (interactive (ag-and-a-half-interactive))
  (ag-and-a-half-run directory regexp pattern))

;;;###autoload
(defun ag-and-a-half-same (pattern &optional regexp directory)
  "Run ag with --type matching the current `major-mode'.
The types of files searched are determined by
`ag-and-a-half-mode-type-alist' and
`ag-and-a-half-mode-extension-alist'.  If no type is configured,
the buffer's file extension is used for the search.  PATTERN is
interpreted as a regular expression, iff REGEXP is non-nil.  If
called interactively, the value of REGEXP is determined by
`ag-and-a-half-regexp-search'.  A prefix arg toggles that value.
DIRECTORY is the directory in which to start searching.  If
called interactively, it is determined by
`ag-and-a-half-project-root-file-patterns`.  The user is only
prompted, if `ag-and-a-half-prompt-for-directory' is set.`"
  (interactive (ag-and-a-half-interactive))
  (let ((type (ag-and-a-half-type)))
    (if type
        (apply 'ag-and-a-half-run directory regexp pattern type)
      (ag-and-a-half pattern regexp directory))))

;;;###autoload
(defun ag-and-a-half-find-file (&optional directory)
  "Prompt to find a file found by ag in DIRECTORY."
  (interactive (list (ag-and-a-half-read-dir)))
  (find-file (expand-file-name
              (ag-and-a-half-read-file
               "Find file: "
               (ag-and-a-half-list-files directory))
              directory)))

;;;###autoload
(defun ag-and-a-half-find-file-same (&optional directory)
  "Prompt to find a file found by ag in DIRECTORY."
  (interactive (list (ag-and-a-half-read-dir)))
  (find-file (expand-file-name
              (ag-and-a-half-read-file
               "Find file: "
               (apply 'ag-and-a-half-list-files directory (ag-and-a-half-type)))
              directory)))

;;; End ag-and-a-half.el ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'ag-and-a-half)

;;; ag-and-a-half.el ends here
