(server-start)
(setq debug-on-error t)

(setq user-full-name "Marc Lehman")
(setq user-mail-address "marc.lehman@xyleminc.com")

(defconst emacs-start-time (current-time))
(unless noninteractive (message "loading %s..." load-file-name))
;; Replace Splash Screen with org-mode scratch buffer ;;
;;(setq inhibit-splash-screen t initial-scratch-message nil initial-major-mode 'org-mode)

(defun mal-get-fullpath (@file-relative-path)
  (concat (file-name-directory (or load-file-name
								   buffer-file-name)) @file-relative-path))

(load-file "~/.emacs.d/git-packages/sensible-defaults/sensible-defaults.el")
(sensible-defaults/use-all-settings)
(sensible-defaults/use-all-keybindings)

;; Package Managers
(defun init-package-manager ()
  "Set up Emacs package manager with use-package."
  (require 'package)
  (setq-default
    load-prefer-newer t
    package-enable-at-startup nil)
  (add-to-list 'package-archives '("marmalade" . "https://marmalade-repo.org/packages/") t)
  (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
  (add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/") t)
  (setq-default package-archive-enable-alist '(("melpa" deft magit)))
  (package-initialize))

(load-file "~/.emacs.d/git-packages/sensible-defaults/sensible-defaults.el")
(sensible-defaults/use-all-settings)
(sensible-defaults/use-all-keybindings)
(init-package-manager)

;; Basic Dependencies
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))


(add-to-list 'load-path "~/.emacs.d/git-packages")

(defconst toc:emacs-config-dir "~/.emacs.d/config/"
  "")
(load-file "~/.emacs.d/config/mal-emacs-lib.el")

;; Emax Binaries
(defvar emax-root (concat (expand-file-name "~") "/emax"))
(defvar emax-bin (concat emax-root "/bin"))
(defvar emax-bin64 (concat emax-root "/bin64"))
(defvar emax-mingw64 (concat emax-root "/mingw64/bin"))
(defvar emax-lisp (concat emax-root "/lisp"))

(setq exec-path (cons emax-bin exec-path))
(setenv "PATH" (concat emax-bin ";" (getenv "PATH")))

(setq exec-path (cons emax-bin64 exec-path))
(setenv "PATH" (concat emax-bin64 ";" (getenv "PATH")))

(setq exec-path (cons emax-mingw64 exec-path))
(setenv "PATH" (concat emax-mingw64 ";" (getenv "PATH")))

(setenv "PATH" (concat "C:\\msys64\\usr\\bin;C:\\msys64\\mingw64\\bin;" (getenv "PATH")))

(dolist (dir '("~/emax/" "~/emax/bin/" "~/emax/bin64/" "~/emax/mingw64/bin/" "~/emax/lisp/" "~/emax/elpa/"))
(add-to-list 'load-path dir))

;; Load my configuration files.
(toc:load-config-file '("mal-emacs-settings.el" "mal-emacs-org.el" "mal-emacs-javascript.el"
						"mal-emacs-perl.el" "mal-emacs-misc.el"))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
	("7527f3308a83721f9b6d50a36698baaedc79ded9f6d5bd4e9a28a22ab13b3cb1" default)))
 '(org-agenda-files (quote ("~/org/Sensus.org")))
 '(package-selected-packages
   (quote
	(org-beautify-theme mbo70s-theme exotica-theme realgud tern magit dash-functional lispyscript-mode esup json-mode solarized-theme org-chef ox-mediawiki ox-minutes ob-mongo jira-markup-mode pandoc-mode markdown-mode markdown-mode+ markdown-preview-mode markdownfmt org-jira ## flycheck flylisp json-navigator indium js-comint elisp-format xref-js2 js2-refactor js2-mode ppd-sr-speedbar neotree nord-theme rebecca-theme cyberpunk-theme use-package rainbow-delimiters prettier-js bind-key f workgroups2))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
