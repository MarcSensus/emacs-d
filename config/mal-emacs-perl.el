;; Replace perl mode with cperl mode globally.
(defalias 'perl-mode 'cperl-mode)

;; Compile support.
(autoload 'mode-compile "mode-compile"
  "Command to compile current buffer file based on the major mode" t)
(global-set-key "\C-cc" 'mode-compile)
(autoload 'mode-compile-kill "mode-compile"
  "Command to kill a compilation launched by `mode-compile'" t)
(global-set-key "\C-ck" 'mode-compile-kill)

(use-package pod-mode
  :ensure t)
(add-to-list 'auto-mode-alist
  '("\\\\.pod$" . pod-mode))
; You might appreciate turning on these
;   features by default for Pod:
(add-hook 'pod-mode-hook '(lambda ( ) (progn
 (font-lock-mode)   ; =syntax highlighting
 (auto-fill-mode 1) ; =wordwrap
 (flyspell-mode 1)  ; =spellchecking
)))

;;; PERL 6 ;;;
(use-package perl6-mode
  :ensure t
  :defer t)

(use-package flycheck-perl6
  :ensure t)
