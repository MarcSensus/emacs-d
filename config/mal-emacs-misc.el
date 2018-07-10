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
  :config (ido-ubiquitous-mode 1))
(use-package
  smex
  :config (smex-initialize)
  :bind ("M-x" . 'smex)
  ("M-X" . 'smex-major-mode-commands)
  ;; This is your old M-x.
  ("C-c C-c M-x" . 'execute-extended-command))
(use-package
  flx-ido
  :config (flx-ido-mode 1)
  ;; disable ido faces to see flx highlights.
  (setq ido-enable-flex-matching t)
  (setq ido-use-faces nil))
(use-package
  ido-vertical-mode
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

;; Workgroup support.
(use-package
  workgroups2
  :ensure t
  :config (setq wg-session-load-on-start nil)
  (workgroups-mode 1))

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
  :config (projectile-global-mode t)
  (global-set-key (kbd "s-c") #'projectile-find-file)
  (help/on-windows
   (setq projectile-indexing-method 'alien))
  :diminish projectile-mode)

;; Helm
(comment
 (use-package
   helm
   :ensure t
   :demand t
   :bind ("C-c i" . 'helm-imenu)
   :config (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebihnd tab to do persistent action
   (define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
   (define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z
   (setq helm-autoresize-max-height 0)
   (setq helm-autoresize-min-height 20)
   (helm-mode 1)
   (helm-autoresize-mode 1)
   :init (progn
		   (require 'helm-config)
		   (setq helm-candidate-number-limit 100)
		   ;; From https://gist.github.com/antifuchs/9238468
		   (setq helm-idle-delay 0.0 ; update fast sources immediately (doesn't).
				 helm-input-idle-delay 0.01	; this actually updates things
                                        ; reeeelatively quickly.
				 helm-yas-display-key-on-candidate t helm-quick-update t helm-M-x-requires-pattern
				 nil helm-ff-skip-boring-files t)
		   (when (executable-find "curl")
			 (setq helm-google-suggest-use-curl-p t))
		   (setq helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
				 helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
				 helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
				 helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
				 helm-ff-file-name-history-use-recentf t helm-echo-input-in-header-line t))))

;; The default "C-x c" is quite close to "C-x C-c", which quits Emacs.
;; Changed to "C-c h". Note: We must set "C-c h" globally, because we
;; cannot change `helm-command-prefix-key' once `helm-config' is loaded.
;;(global-set-key (kbd "C-c h") 'helm-command-prefix)
;;(global-unset-key (kbd "C-x c"))
;;(global-set-key (kbd "C-x C-f") 'helm-find-files)

;; Elixir Programming
(require 'flycheck-elixir)
(add-hook 'elixir-mode-hook 'flycheck-mode)
(use-package
  elixir-mode
  :ensure t
  :config (add-hook 'elixir-mode-hook
					(lambda ()
					  (add-hook 'before-save-hook 'elixir-format nil t))))
(use-package
  flycheck-elixir
  :config (add-hook 'elixir-mode-hook 'flycheck-mode))

;; NewLISP Programming
(use-package
  newlisp-mode)
