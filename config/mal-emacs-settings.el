(defun editor-settings ()
  "Settings for all modes."
  (set-language-environment "UTF-8")
  (show-paren-mode 1)
  (setq-default require-final-newline t)
  (global-subword-mode 1)
  (global-prettify-symbols-mode t)
  (setq-default show-paren-delay 0)
  (setq scroll-conservatively 100)
  (setq compilation-scroll-output t)
  (setq x-select-enable-clipboard t)
  ;;Indentation rules.
  (setq-default indent-tabs-mode t)
  (setq-default tab-width 4)
  (setq tab-stop-list (number-sequence 4 200 4))
  ;; Show column number and line number.
  (setq-default column-number-indicator-zero-based nil)
  (dolist (mode '(column-number-mode line-number-mode))
	(when (fboundp mode)
	  (funcall mode t)))
  (dolist (mode-hook '(text-mode-hook prog-mode-hook))
	(add-hook mode-hook
			  (lambda ()
				(setq display-line-number-mode t)))))

(defun gui-settings ()
  "GUI settings for Emacs."
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (global-linum-mode t)
  (blink-cursor-mode -1)
  (fringe-mode 6)
  (global-hl-line-mode t)
  (setq ring-bell-function 'ignore)
  (setq frame-title-format '(""
							 (:eval (if (buffer-file-name)
										(abbreviate-file-name (buffer-file-name)) "%b")))))

;; Load the main editor settings.
(editor-settings)
(gui-settings)

;; File Backup
(setq backup-by-copying t							; don't clobber symlinks
	  backup-directory-alist '(("." . "~/.saves/"))	; don't litter my fs tree
	  delete-old-versions t kept-new-versions 6 kept-old-versions 2 version-control t) ; use versioned backups

;; Key Bindings
(global-set-key (kbd "C-x 2") 'split-window-below-and-switch)
(global-set-key (kbd "C-x 3") 'split-window-right-and-switch)
(global-set-key (kbd "C-x p") 'eval-buffer)
(global-set-key (kbd "C-x C-g") 'rgrep)
(global-set-key (kbd "M-o") 'other-window)
(global-set-key (kbd "C-x o") 'window-swap-states)
(global-set-key (kbd "M-p") 'cperl-perldoc)
;;(global-set-key (kbd "C-c i") 'helm-imenu)

;; Font Settings
;;(set-frame-font "Fira Code 10" nil t)
;;(setq-default default-font "Fira Code")
(setq-default default-font "Hack")
(setq-default default-font-size 10)
(set-font-size)
(when window-system (set-frame-size (selected-frame) 180 50))

;; Theme
(when window-system
  ;;(apply-theme 'set-solarized-theme)
  ;;(apply-theme 'set-nord-theme)
  (apply-theme 'set-rebecca-theme)
  ;;(apply-theme 'set-cyberpunk-theme)
  ;;(apply-theme 'set-70s-theme)
  ;;(apply-theme 'set-exotica-theme)
  (apply-theme 'set-org-beautify-theme)
  )

;; Windows Specific
(help/on-windows
 (setq shell-file-name "cmdproxy.exe"))

(help/on-windows
 (setq w32-pass-lwindow-to-system nil)
 (defvar w32-lwindow-modifier 'super)
 (setq w32-pass-rwindow-to-system nil)
 (defvar w32-rwindow-modifier 'super))

(help/on-windows
  (set-clipboard-coding-system 'utf-16le-dos))

;; Startup
(split-window-horizontally)
(add-hook 'window-setup-hook 'toggle-frame-maximized t)
