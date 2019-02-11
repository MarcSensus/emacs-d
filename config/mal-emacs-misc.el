;; Speed Bars
(use-package
  neotree
  :ensure t
  :bind (("<f8>" . neotree-toggle)))
(use-package
  sr-speedbar
  :ensure t
  :bind (("<f9>" . sr-speedbar-toggle)))

;; IDO Mode
(require 'ido)
(ido-mode 1)
;;(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(use-package
  ido-completing-read+
  :ensure t
  :config (ido-ubiquitous-mode 1))
(use-package
  smex
  :ensure t
  :config (smex-initialize)
  :bind (("M-x" . smex)
		 ("M-X" . smex-major-mode-commands))
  ;; This is your old M-x.
  ;; ("C-c C-c M-x" . 'execute-extended-command)
  )
(use-package
  flx-ido
  :ensure t
  :config (flx-ido-mode 1)
  ;; disable ido faces to see flx highlights.
  (setq ido-enable-flex-matching t)
  (setq ido-use-faces nil))
(use-package
  ido-vertical-mode
  :ensure t
  :config (ido-vertical-mode 1)
  (setq ido-vertical-define-keys 'C-n-and-C-p-only))
(require 'icomplete)
(icomplete-mode 1)

(use-package
  markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
		 ("\\.md\\'" . markdown-mode)
		 ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

(use-package
  elisp-format
  :ensure t)

(use-package
  transpose-frame
  :ensure t)

;; Rainbow Delimeters
(use-package
  rainbow-delimiters
  :ensure t
  :init (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
  (add-hook 'elisp-mode-hook 'rainbow-delimiters-mode)
  (add-hook 'php-mode-hook 'rainbow-delimiters-mode)
  (add-hook 'css-mode-hook 'rainbow-delimiters-mode)
  (add-hook 'web-mode-hook 'rainbow-delimiters-mode)
  (add-hook 'js-mode-hook 'rainbow-delimiters-mode)
  (add-hook 'rust-mode-hook 'rainbow-delimiters-mode))

(use-package
  dashboard
  :config (dashboard-setup-startup-hook))

;; ;; Workgroup support.
;; (use-package
;;   workgroups2
;;   :ensure t
;;   :config (setq wg-session-load-on-start nil)
;;   (workgroups-mode 1))

;; EWW
(defun my/eww-toggle-images ()
  "Toggle whether images are loaded and reload the current page from cache."
  (interactive)
  (setq-local shr-inhibit-images (not shr-inhibit-images))
  (eww-reload t)
  (message "Images are now %s" (if shr-inhibit-images "off" "on")))
;;(define-key eww-mode-map (kbd "I") #'my/eww-toggle-images)
;;(define-key eww-link-keymap (kbd "I") #'my/eww-toggle-images)
;; minimal rendering by default
(setq-default shr-inhibit-images t)		; toggle with `I`
(setq-default shr-use-fonts nil)		; toggle with `F`

;; Magit
(use-package
  magit
  :ensure t
  :bind (("C-x g" . magit-status)))

(use-package
  projectile
  :ensure t
  :config (projectile-mode t)
  (global-set-key (kbd "s-c") #'projectile-find-file)
  (help/on-windows
   (setq projectile-indexing-method 'alien))
  :diminish projectile-mode)

;; (comment(use-package
;;   window-purpose
;;   :ensure t
;;   :config (purpose-mode)))

;; (comment(use-package
;;   which-key
;;   :ensure t
;;   :config (which-key-mode)
;;           (setq purpose-user-mode-purposes
;;             '((org-mode . org)
;;               (eshell-mode . terminal)
;;               (shell-mode . terminal)
;;               (go-mode . edit)
;;               (markdown-mode . org)))
;;           (purpose-compile-user-configuration)))

;; Elixir Programming
(use-package
  elixir-mode
  :ensure t
  :config (add-hook 'elixir-mode-hook
					(lambda ()
					  (add-hook 'before-save-hook 'elixir-format nil t))))
(use-package
  flycheck-elixir
  :ensure t
  :config (add-hook 'elixir-mode-hook 'flycheck-mode))
(use-package
  alchemist
  :ensure t)

;; NewLISP Programming
(use-package
  newlisp-mode
  :ensure t)

;; Calendar Config
(copy-face font-lock-constant-face 'calendar-iso-week-face)
(set-face-attribute 'calendar-iso-week-face nil
					:height 1.0
					:foreground "salmon")
(setq calendar-intermonth-text '(propertize (format "%2d" (car (calendar-iso-from-absolute
																(calendar-absolute-from-gregorian
																 (list month day year)))))
											'font-lock-face 'calendar-iso-week-face))
