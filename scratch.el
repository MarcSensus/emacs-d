;;; scratch.el --- Emacs Lisp Scratch -*- lexical-binding: t -*-

;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; scratch.el ends here

;; Theme testing.
(apply-theme 'set-solarized-theme)
(apply-theme 'set-nord-theme)
(apply-theme 'set-rebecca-theme)
(apply-theme 'set-cyberpunk-theme)
(apply-theme 'set-70s-theme)
(apply-theme 'set-exotica-theme)
(apply-theme 'set-org-beautify-theme)
(disable-theme <theme-in-use>)
(load-file "~/.emacs.d/git-packages/weyland-yutani-theme.el")
(load-locked-desktop)

;; Real scratch under here.
