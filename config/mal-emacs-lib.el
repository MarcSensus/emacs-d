;; In text mode, I don't want it auto-indenting for the first
;; line in the file, or lines following blank lines.
;; Everywhere else is okay.
(defun newline-and-text-indent ()
  "Insert a newline, then indent the next line sensibly for text"
  (interactive)
  (cond
   ;; Beginning of buffer, or beginning of an existing line, don't indent:
   ((or
	 (bobp)
	 (bolp))
	(newline))

   ;; If we're on a whitespace-only line,
   ((and
	 (eolp)
	 (save-excursion (re-search-backward "^\\(\\s \\)*$" (line-beginning-position) t)))
    ;; ... delete the whitespace, then add another newline:
    (kill-line 0)
	(newline))

   ;; Else (not on whitespace-only) insert a newline,
   ;; then add the appropriate indent:
   (t (insert "\n")
	  (indent-according-to-mode))))

(defun my-custom-settings-fn ()
  (setq indent-tabs-mode t)
  (setq tab-stop-list (number-sequence 4 200 4))
  (setq tab-width 4)
  (setq indent-line-function 'newline-and-text-indent)
  (local-set-key "\C-m" 'newline-and-text-indent))
;;(add-hook 'text-mode-hook 'my-custom-settings-fn)

;; utility function to auto-load my package configurations
(defun toc:load-config-file (filelist)
  (dolist (file filelist)
	(load (expand-file-name (concat toc:emacs-config-dir file)))
	(message "Loaded config file:%s" file)))

;; Misc util functions
(defun split-window-below-and-switch ()
  "Split the window horizontally and then switch to the new pane."
  (interactive)
  (split-window-below)
  (other-window 1))

(defun split-window-right-and-switch ()
  "Split the window vertically and then switch to the new pane."
  (interactive)
  (split-window-right)
  (other-window 1))

(defun apply-theme (theme-function)
  "Takes the theme set up function and apply it to the proper environemnts.
THEME-FUNCTION: function that initializes the themes and settings."
  (when window-system
    (funcall theme-function))
  (if (daemonp)
      (add-hook 'after-make-frame-functions
                (lambda (frame)
                  (funcall theme-function)))
    (funcall theme-function)))

(defun font-code ()
  "Return a string representing the current font (like \"Inconsolata-14\")."
  (concat default-font "-" (number-to-string default-font-size)))

(defun set-font-size ()
  "Set the font to `default-font' at `current-font-size'.
Set that for the current frame, and also make it the default for
other, future frames."
  (let ((font-code (font-code)))
    (add-to-list 'default-frame-alist (cons 'font font-code))
    (set-frame-font font-code)))

; THEMES
(defun set-solarized-theme ()
  "Set up and run the solarized theme."
  (use-package solarized-theme
    :ensure t
    :pin melpa-stable
    :init
    (fringe-mode 15)
    (setq-default solarized-use-variable-pitch nil)
    (setq-default solarized-high-contrast-mode-line nil)
    (setq-default solarized-use-less-bold t)
    (setq-default solarized-use-more-italic nil)
    (setq-default solarized-emphasize-indicators t)
    (setq-default solarized-scale-org-headlines nil)
    (setq x-underline-at-descent-line t)
    :config
    (load-theme 'solarized-dark t)))

(defun set-nord-theme ()
  "Set up and run the nord theme."
  (use-package nord-theme
    :ensure t
    :pin melpa-stable
    :init
    (setq-default node-uniform-mode-lines t)
    (setq-default nord-comment-brightness 15)
    (setq-default node-region-highlight "frost")
    :config
    (load-theme 'nord t)))

(defun set-rebecca-theme ()
  "Set up and run the rebecca theme."
  (use-package rebecca-theme
    :ensure t
    :pin melpa-stable
    :config
    (load-theme 'rebecca t)))

(defun set-cyberpunk-theme ()
  "Set up and run the rebecca theme."
  (use-package cyberpunk-theme
    :ensure t
    :pin melpa-stable
    :config
    (load-theme 'cyberpunk t)))