
(use-package
  js2-mode
  :ensure t
  :config (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
  (setq js2-strict-missing-semi-warning nil)
  (setq js2-missing-semi-one-line-override t)
  (add-hook 'js2-mode-hook #'js2-imenu-extras-mode)
  (add-hook 'js2-mode-hook
			(lambda ()
			  (local-set-key (kbd "C-x C-e") 'js-send-last-sexp)
			  (local-set-key (kbd "C-M-x") 'js-send-last-sexp-and-go)
			  (local-set-key (kbd "C-c b") 'js-send-buffer)
			  (local-set-key (kbd "C-c C-b") 'js-send-buffer-and-go)
			  (local-set-key (kbd "C-c l") 'js-load-file-and-go))))

(use-package
  js2-refactor
  :ensure t
  :config
  (add-hook 'js2-mode-hook #'js2-refactor-mode)
  (js2r-add-keybindings-with-prefix "C-c C-r")
  (define-key js2-mode-map (kbd "C-k") #'js2r-kill))

(use-package
  xref-js2
  :ensure t
  :config
  ;; js-mode (which js2 is based on) binds "M-." which conflicts with xref, so
  ;; unbind it.
  (define-key js-mode-map (kbd "M-.") nil)
  (add-hook 'js2-mode-hook
			(lambda ()
			  (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t))))

(use-package
  js-comint
  :ensure t
  :config (setq inferior-js-program-command "node"))

(use-package
  tern
  :ensure t
  :config (add-hook 'js2-mode-hook (lambda () (tern-mode t))))

(use-package
  flycheck
  :ensure t
  :init (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))
  (setq-default js2-mode-show-parse-errors nil)
  (setq-default js2-mode-show-strict-warnings nil)
  :config (add-hook 'after-init-hook #'global-flycheck-mode)
  '((js2-mode (flycheck-checker . javascript-standard)))
  '(flycheck-select-checker 'javascript-standard))

(use-package
  prettier-js
  :ensure t
  :config (setq prettier-js-args '("--print-width" "80" "--use-tabs" "true" "--no-semi" "true"
								   "--single-quote" "true"))
  (add-hook 'js2-mode-hook 'prettier-js-mode))

(setq inferior-js-mode-hook
	  (lambda ()
		;; We like nice colors
		(ansi-color-for-comint-mode-on)
		;; Deal with some prompt nonsense
		(add-to-list 'comint-preoutput-filter-functions
					 (lambda (output)
					   (replace-regexp-in-string ".*1G\.\.\..*5G" "..." (replace-regexp-in-string
																		 ".*1G.*3G" "&gt;" output))))))
