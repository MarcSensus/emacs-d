;;; lpkg-explorer.el --- Emacs loaded packages exploration tools  -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Michelle Baert

;; Author:  Michelle Baert https://gist.github.com/RockyRoad29
;; Keywords: maint
;; ---------------------------------------------------------------
;;
;; These functions are designed basically for use in a lisp interaction session,
;; where more variants may easily be tailored for your needs .
;;
;; My main purpose here is to detect potential org-mode conflicts between
;; built-in (typically older) and user local versions.
;;
;; Although it would be a nice use-case for org-babel evaluation for reporting,
;; I need something working when org-babel is broken.

;; ---------------------------------------------------------- Initialization
;; extract data from builtin var 'load-history
(setq tmp:lpkg-load-history-files nil)

;; set up some regexp for paths recognition
(setq tmp:builtin-path-re "^/usr/share/emacs/\\(.*\\)/\\(.*\\)")
(setq tmp:local-path-re
      "^/home/.*?/\\.emacs.*?\\.d.*?/\\(.*\\)/\\(.*\\)"
)
(setq tmp:local-path-re2
      (concat "^"
              (expand-file-name user-emacs-directory)
              "elpa/\\(.*\\)/\\(.*\\)"))

(defun tmp:lpkg-init(&optional description append)
  "Creates a dedicated buffer for output, and print some headers there."
  (setq tmp:lpkg-load-history-files
        (reverse (seq-filter #'stringp
                             (mapcar #'car load-history)
                             )))
  (save-selected-window
    (pop-to-buffer "*loaded-packages*")
    (if append
        (goto-char (point-max))
      (erase-buffer))
    (insert "* Locating Loaded Packages\n\n")
    (if description (insert description "\n"))
    (unless append
      (insert
       "\n#+BEGIN_EXPORT markdown\n"
       (spacemacs//describe-system-info-string)
       "#+END_EXPORT\n\n"))
    (insert (format
             "%d total modules currently loaded, %d registered features\n\n"
             (length tmp:lpkg-load-history-files)
             (length features)
             ))
    )
  )

;; ----------------------------------------------------------- Toolbox

(defun tmp:lpkg-list-items (l1 header format-item)
  "Appends a formatted list to the dedicated buffer"
  (with-current-buffer (get-buffer-create "*loaded-packages*")
    (goto-char
     (set-window-point (get-buffer-window (current-buffer))
                       (point-max)))
    (insert (format "\n** %s\n" header))
    (dolist (x l1)
      (insert (format"  - %s\n" (funcall format-item x)))
      )
    t
    ))

(defun tmp:lpkg-table-items (l1 header cells-for-item)
  "Appends a formatted table to the dedicated buffer"
  (with-current-buffer (get-buffer-create "*loaded-packages*")
    (goto-char (point-max))
    (insert (format "\n** %s\n" header))
    (dolist (x l1)
      (insert (format"  | %s |\n"
                     (mapconcat #'identity
                                (funcall cells-for-item x)
                                " | ")))
      )
    t
    ))

;; Provide a simple org-babel block to evaluate
(defun tmp:lpkg-append-text (&rest expr-list)
  "Appends any text to the dedicated buffer"
  (with-current-buffer (get-buffer-create "*loaded-packages*")
    (goto-char (point-max))
    (apply #'insert expr-list)
    )
  )

(defun tmp:lpkg-print-modules ()
  (tmp:lpkg-list-items
   tmp:lpkg-load-history-files
   (format "%d modules loaded" (length tmp:lpkg-load-history-files))
   #'identity)
    )

(defun tmp:lpkg-print-matching (regexp)
  (let (
        (l1 (seq-filter
             (lambda (x) (and x (string-match regexp x)))
             tmp:lpkg-load-history-files))
        )
    (tmp:lpkg-table-items
     l1
     (format "%d modules matching %s (%d%%)"
             (length l1)
             (prin1-to-string regexp)
             (/ (* 100 (length l1)) (length tmp:lpkg-load-history-files)))
     #'tmp:lpkg-parse-library-name)
    ))

(defun tmp:lpkg-list-features ()
  (tmp:lpkg-list-items
   features
   (format "%d features currently loaded"
           (length features)
           )
   (lambda (x)
     (format "%s: %s" x (find-library-name (symbol-name x))))
   ))

(defun tmp:lpkg-table-features ()
  (tmp:lpkg-table-items
   features
   (format "%d features currently loaded"
           (length features)
           )
   (lambda (x)
     (let ((name (symbol-name x)))
       (list name (find-library-name name))
     ))
  ))


(defun tmp:lpkg-list-locations-for-features-matching (regexp)
  ""
  (let (
        (l1 (seq-filter
             (lambda (x) (string-match regexp x))
             (mapcar #'symbol-name features) ))
        )
    (tmp:lpkg-list-items l1
     (format "%d features matching %s (%d%%)"
             (length l1)
             (prin1-to-string regexp)
             (/ (* 100 (length l1)) (length features)))
     (lambda (x)
       (format "%s: %s" x (find-library-name x)))
     ))
  )
(defun tmp:matched-groups (string)
  (do* (
        (n 1 (1+ n))
        (grp (match-string n string) (match-string n string))
        (groups nil)
        )
      (
       (or (null grp)(> n 3))
       (reverse groups)
       )
    ;; (princ (format "%d: %s\n" n grp))
    (setq groups (push grp groups))
    )
  )
(defun tmp:lpkg-parse-library-name(lib)
  (cond
   ((string-match tmp:builtin-path-re lib)
    (cons "BUILTIN" (tmp:matched-groups lib))
    )
   ((string-match tmp:local-path-re lib)
    (cons "LOCAL" (tmp:matched-groups lib))
    )
   (t (list "?" lib))
   )
  )

(defun tmp:lpkg-table-features-matching (regexp)
  ""
  (let (
        (l1 (seq-filter
             (lambda (x) (string-match regexp x))
             (mapcar #'symbol-name features) ))
        )
    (tmp:lpkg-table-items (reverse l1)
                         (format "%d features matching %s (%d%%)"
                                 (length l1)
                                 (prin1-to-string regexp)
                                 (/ (* 100 (length l1)) (length features)))
                         (lambda (x)
                           (let ((lib (find-library-name x)) )
                             (cons x (tmp:lpkg-parse-library-name lib))
                             ))
                         ))
  )

;; ----------------------------------------------------------- Examples

;; force load some packages by feature name
;; (require 'ob)
;; (require 'org)
;; (require 'org-agenda)
;; (require 'org-mime)

;; check where they came from
(tmp:lpkg-init)
;(tmp:lpkg-init "Second try" t)
;; (pop-to-buffer "*loaded-packages*" )

(tmp:lpkg-list-items
 (seq-filter
  (lambda(x)(string-match "/o" x))
  load-path)
 "Load Paths entries matching /o*"
 #'identity
 )

(tmp:lpkg-print-matching "/org")
(tmp:lpkg-print-matching "/ob")

;; (tmp:lpkg-print-matching "/\\(org\\|ob\\)")

(tmp:lpkg-table-features-matching "^org\\|ob")


;; Checking org version
(tmp:lpkg-list-items '(
                       (org-version)
                       (functionp 'org-link-types)
                       (functionp 'org-babel-check-confirm-evaluate)
                       (functionp 'org-element--set-regexps)
                       )
                     "Org version hints"
                     (lambda (x)
                       (format "%s ;--> %s" x (eval x)))
                     )

;; Try to find source code
(tmp:lpkg-list-items '(
                       'org-version
                       'org-element--set-regexps
                       'org-link-types
                       'org-babel-check-confirm-evaluate
                       'org-babel-check--evaluate
                       'org-babel-execute-src-block
                       )
                     "Trying to find source code
These are an attempt to get source code as in the link in
a ##'describe-function= buffer. It doesn't always work but it's worth a try.

In org 8.2 ,'org-babel-check-confirm-evaluate and 'org-babel-check--evaluate
are inline functions (defsubst) called by 'org-babel-execute-src-block , all in ob--core.el .
"
                     (lambda (expr)
                       (let ((sym (eval expr))f)
                         (cond
                          ((functionp sym)
                           (describe-function sym)
                           (setq f (find-lisp-object-file-name sym t))
                           (format "'%s is a function in %S" sym f)
                           )
                          ((fboundp sym)
                           (describe-function sym)
                           (setq f (find-lisp-object-file-name sym t))
                           (format "'%s is a bound function in %S" sym f)
                           )
                          ((macrop sym)
                           (describe-variable sym)
                           (setq f (find-lisp-object-file-name sym t))
                           (format "'%s is a macro in %S" sym f)
                           )
                          ((boundp sym)
                           (describe-variable sym)
                           (setq f (find-lisp-object-file-name sym 'defvar))
                           (format "'%s is a variable in %S" sym f)
                           )
                          ((symbolp sym)
                           (describe-symbol sym)
                           (format "'%s is another symbol" sym)
                           )
                          )
                         )
                       )
                     )

;; Provide a simple org-babel block to evaluate
(tmp:lpkg-append-text
"

** org-babel evaluation
Put this buffer in *org-mode* and try to
evaluate this simple block of emacs-lisp code:

Sample elisp block for org-babel evaluation
 #+BEGIN_SRC elisp
 \"Hello World\"
 #+END_SRC

Note that is now marked as 'elisp instead of 'emacs_lisp for org 9.0.x
#+BEGIN_SRC emacs_lisp
 \"Hello World\"
#+END_SRC

"
 )
