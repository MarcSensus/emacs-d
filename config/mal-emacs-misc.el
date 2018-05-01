;; Speed Bars
(use-package
  neotree
  :ensure t
  :bind (("<f8>" . neotree-toggle)))
(use-package
  sr-speedbar
  :ensure t
  :bind (("<f9>" . sr-speedbar-toggle)))

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

;; Workgroup support.
(use-package
  workgroups2
  :ensure t
  :config
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
