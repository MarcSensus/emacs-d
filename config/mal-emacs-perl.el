;;; All Dev Modes ;;;


;;; PERL 5 ;;;
;; Replace perl mode with cperl mode globally.
(defalias 'perl-mode 'cperl-mode)

;; Compile support.
(autoload 'mode-compile "mode-compile"
  "Command to compile current buffer file based on the major mode" t)
(global-set-key "\C-cc" 'mode-compile)
(autoload 'mode-compile-kill "mode-compile"
  "Command to kill a compilation launched by `mode-compile'" t)
(global-set-key "\C-ck" 'mode-compile-kill)

(use-package
  pod-mode
  :ensure t)
(add-to-list 'auto-mode-alist '("\\\\.pod$" . pod-mode))
										; You might appreciate turning on these
										;   features by default for Pod:
(add-hook 'pod-mode-hook
		  '(lambda ( )
			 (progn (font-lock-mode)	; =syntax highlighting
					(auto-fill-mode 1)	; =wordwrap
					(flyspell-mode 1)	; =spellchecking
					)))

;;; PERL 6 ;;;
(use-package
  perl6-mode
  :ensure t
  :defer t)

(use-package
  flycheck-perl6
  :ensure t)

;;; GO ;;;
;; Define function to call when go-mode loads
(defun my-go-mode-hook ()
  (add-hook 'before-save-hook 'gofmt-before-save) ; gofmt before every save
  (setq gofmt-command "goimports")                ; gofmt uses invokes goimports
  (if (not (string-match "go" compile-command))   ; set compile command default
      (set (make-local-variable 'compile-command) "go build -v && go test -v && go vet"))

  ;; guru settings
  (go-guru-hl-identifier-mode)			; highlight identifiers

  ;; Key bindings specific to go-mode
  (local-set-key (kbd "M-.") 'godef-jump)	  ; Go to definition
  (local-set-key (kbd "M-*") 'pop-tag-mark)	  ; Return from whence you came
  (local-set-key (kbd "M-p") 'compile)		  ; Invoke compiler
  (local-set-key (kbd "M-P") 'recompile)	  ; Redo most recent compile cmd
  (local-set-key (kbd "M-]") 'next-error)	  ; Go to next error (or msg)
  (local-set-key (kbd "M-[") 'previous-error) ; Go to previous error or msg

  ;; Misc go stuff
  (auto-complete-mode 1))				; Enable auto-complete mode

;; Connect go-mode-hook with the function we just defined
(add-hook 'go-mode-hook 'my-go-mode-hook)

;; Ensure the go specific autocomplete is active in go-mode.
(with-eval-after-load 'go-mode
  (require 'go-autocomplete))

;; If the go-guru.el file is in the load path, this will load it.
(require 'go-guru)

;;; Clojure ;;;
(use-package
  cider
  :ensure t)

;;; Lisp ;;;
(use-package srefactor
  :ensure t
  :config
  (require 'srefactor-lisp)
  (semantic-mode 1) ;; -> this is optional for Lisp

  (define-key c-mode-map (kbd "M-RET") 'srefactor-refactor-at-point)
  (define-key c++-mode-map (kbd "M-RET") 'srefactor-refactor-at-point)
  (global-set-key (kbd "M-RET o") 'srefactor-lisp-one-line)
  (global-set-key (kbd "M-RET m") 'srefactor-lisp-format-sexp)
  (global-set-key (kbd "M-RET d") 'srefactor-lisp-format-defun)
  (global-set-key (kbd "M-RET b") 'srefactor-lisp-format-buffer))

;;; Docker ;;;
(use-package
  docker-compose-mode
  :ensure t)
(use-package
  dockerfile-mode
  :ensure t
  :mode "Dockerfile\\'"
  :interpreter "dockerfile")

;;; R and Statistics ;;;
(use-package
  ess
  :ensure t
  :init (require 'ess-site)
  :config
  (setq ess-S-assign-key (kbd "C-="))
  (ess-toggle-S-assign-key t) ; enable above key definition
  ;; leave my underscore key alone!
  (ess-toggle-underscore nil))

;;; CMake ;;;
(use-package
  cmake-mode
  :ensure t)

;;; C# ;;;
(use-package
  csharp-mode
  :ensure t
  :mode ("\\.cs\\'" . csharp-mode))

(use-package
  omnisharp
  :ensure t
  :hook (csharp-mode . omnisharp-mode)
  :config
  (eval-after-load 'company '(add-to-list 'company-backends #'company-omnisharp)))

(use-package
  dotnet
  :ensure t
  :hook (csharp-mode . dotnet-mode))

(add-to-list 'auto-mode-alist
			 '("\\.csproj\\'" . (lambda () (xml-mode))))

(provide 'init-dotnet)

;;; Universal ;;;
(use-package
  format-all
  :ensure t)

(use-package
  dumb-jump
  :ensure t
  :config
  (dumb-jump-mode))
