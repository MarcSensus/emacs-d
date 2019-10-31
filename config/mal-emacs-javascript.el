(setq js-indent-level 2)

(use-package
  js2-mode
  :ensure t
  :config (add-to-list 'auto-mode-alist '("\\.js\\'" . js-mode))
  (add-hook 'js-mode-hook 'js2-minor-mode)
  (setq js2-strict-missing-semi-warning nil js2-missing-semi-one-line-override t
		js2-skip-preprocessor-directives t js-chain-indent t
        ;; let flycheck handle this
        js2-mode-show-parse-errors nil js2-mode-show-strict-warnings nil
        ;; Flycheck provides these features, so disable them: conflicting with
        ;; the eslint settings.
        js2-strict-trailing-comma-warning nil js2-strict-missing-semi-warning nil
        ;; maximum fontification
        js2-highlight-level 3 js2-highlight-external-variables t)
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
  :config (add-hook 'js2-mode-hook #'js2-refactor-mode)
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
  add-node-modules-path
  :ensure t
  :config (add-hook 'js-mode-hook #'add-node-modules-path))

(use-package
  js-comint
  :ensure t
  :config (setq inferior-js-program-command "node"))

(use-package
  tern
  :ensure t
  :config (add-hook 'js-mode-hook
					(lambda ()
					  (tern-mode t))))

(use-package
  flycheck
  :ensure t
  :init (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))
  (setq-default js-mode-show-parse-errors nil)
  (setq-default js-mode-show-strict-warnings nil)
  :config (add-hook 'after-init-hook #'global-flycheck-mode))

(use-package
  prettier-js
  :ensure t
  :config (setq prettier-js-args '("--print-width" "100" "--use-tabs" "false" "--no-semi" "true"
								   "--single-quote" "true"))
  ;;(add-hook 'js2-mode-hook 'prettier-js-mode)
  )

;; Org mode bindings for literate programming.
;; (comment(use-package
;;   ob-js
;;   :ensure t))

;; NPM mode and bindings.
(use-package
  npm-mode
  :ensure t)

(use-package
  realgud
  :ensure t)

(setq inferior-js-mode-hook
	  (lambda ()
		;; We like nice colors
		(ansi-color-for-comint-mode-on)
		;; Deal with some prompt nonsense
		(add-to-list 'comint-preoutput-filter-functions
					 (lambda (output)
					   (replace-regexp-in-string ".*1G\.\.\..*5G" "..." (replace-regexp-in-string
																		 ".*1G.*3G" "&gt;" output))))))
