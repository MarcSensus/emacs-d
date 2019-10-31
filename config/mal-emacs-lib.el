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

;; Helper to only execute some things on Windows.
(defmacro help/on-windows (statement &rest statements)
  "Evaluate the enclosed body only when run on Microsoft Windows."
  `(when (eq system-type 'windows-nt) ,statement ,@statements))

;; Macro to add block comments that for some reason elisp doesn't have.
(defmacro comment
	(&rest
	 body)
  "Comment out one or more s-expressions."
  nil)

;; Utility function to auto-load my package configurations.
(defun toc:load-config-file (filelist)
  (dolist (file filelist)
	(load (expand-file-name (concat toc:emacs-config-dir file)))
	(message "Loaded config file:%s" file)))

;; Load locked desktop
(defun load-locked-desktop ()
  (setq desktop-load-locked-desktop t)
  (call-interactively 'desktop-read t (vector "~/.emacs.d/desktops/" t)))

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

;;; Internet Time
; version 3
; written by Mario Lang

(defun itime-internal (hour minute second offset)
 "Return internet time as a float.
HOUR MINUTE and SECOND are local time. OFFSET is the offset in seconds."
  (let* ((seconds (+ (* 3600 hour)
		     3600
		     (- offset)
		     (* 60 minute)
		     second))
	 (beats (/ seconds 86.4)))
    (if (< beats 0)
	(+ 0 beats)
    beats)))

;(assert (= (itime-internal 0 0 0 3600) 0))
;(assert (= (round (itime-internal 12 0 1 3600)) 500))
;(assert (= (round (itime-internal 23 0 0 3600))
;	  (round (itime-internal 0 0 0 7200))))

(defun itime-string (hour minute second &optional ticks)
 "Return internet time formatted as a string.
If TICKS is non-nil, also include the decimal points."
 (let ((result (itime-internal (if (stringp hour)
				   (string-to-number hour)
				 hour)
			       (if (stringp minute)
				   (string-to-number minute)
				 minute)
			       (if (stringp second)
				   (string-to-number second)
				 second)
			       (car (current-time-zone)))))
   (if ticks
       (format "@%03d.%02d"
	       (floor result)
	       (floor (* (- result (floor result)) 100)))
       (format "@%03d" (round result)))))

(defun itime-string-simple (hour minute second &optional ignore)
  "Return internet time as string.
    HOUR MINUTE and SECOND are strings as provided within
    `display-time-string-forms' and are local time."
  (let* ((seconds (+ (* 3600 (string-to-number hour))
		     3600
		     (- (car (current-time-zone)))
		     (* 60 (string-to-number minute))
		     (string-to-number second)))
	 (beats (mod (floor seconds 86.4) 1000)))
    (format "@%03d" beats)))

(defun itime-translate (beats)
  "Translate BEATS from internet time to local time."
  (when (stringp beats)
    (setq beats (string-to-number beats)))
  (let* ((seconds (- (* beats 86.4)
		     3600
		     (- (car (current-time-zone)))))
	 (hours (mod (floor seconds 3600) 24))
	 (minutes (mod (round seconds 60) 60)))
    (format "%d:%02d" hours minutes)))

(defun itime-message (beats)
  "Show BEATS in local time as a message."
  (message (itime-translate beats)))

(defun itime-correct ()
  "When point is on or after a local time, translate it into Internet Time."
  (interactive)
  (skip-chars-backward "0-9: \t")
  (skip-chars-forward " \t")
  (when (looking-at "\\([0-9]+\\):\\([0-9]+\\):?\\([0-9]+\\)?")
    (let ((hours (match-string 1))
	  (minutes (match-string 2))
	  (seconds (match-string 3)))
      (when (not seconds)
	(setq seconds "0"))
      (replace-match (itime-string hours minutes seconds)))))

(defun apply-theme (theme-function)
  "Takes the theme set up function and apply it to the proper environemnts.
THEME-FUNCTION: function that initializes the themes and settings."
  (when window-system (funcall theme-function))
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
  (use-package
	solarized-theme
	:ensure t
	:pin melpa-stable
	:init (fringe-mode 15)
	(setq-default solarized-use-variable-pitch nil)
	(setq-default solarized-high-contrast-mode-line nil)
	(setq-default solarized-use-less-bold t)
	(setq-default solarized-use-more-italic nil)
	(setq-default solarized-emphasize-indicators t)
	(setq-default solarized-scale-org-headlines nil)
	(setq x-underline-at-descent-line t)
	:config (load-theme 'solarized-dark t)))

(defun set-nord-theme ()
  "Set up and run the nord theme."
  (use-package
	nord-theme
	:ensure t
	:pin melpa-stable
	:init (setq-default node-uniform-mode-lines t)
	(setq-default nord-comment-brightness 15)
	(setq-default node-region-highlight "frost")
	:config (load-theme 'nord t)))

(defun set-rebecca-theme ()
  "Set up and run the rebecca theme."
  (use-package
	rebecca-theme
	:ensure t
	:pin melpa-stable
	:config (load-theme 'rebecca t)))

(defun set-cyberpunk-theme ()
  "Set up and run the cyberpunk theme."
  (use-package
	cyberpunk-theme
	:ensure t
	:pin melpa-stable
	:config (load-theme 'cyberpunk t)))

(defun set-exotica-theme ()
  "Set up and run the exotica theme."
  (use-package
	exotica-theme
	:ensure t
	:config (load-theme 'exotica t)))

(defun set-70s-theme ()
  "Set up and run the mbo70s theme."
  (use-package
	mbo70s-theme
	:ensure t
	:config (load-theme 'mbo70s t)))

(defun set-org-beautify-theme ()
  "Set up and run the org stacking theme."
  (use-package
	org-beautify-theme
	:ensure t
	:config (load-theme 'org-beautify t)))

(defun set-wy-theme ()
  (load-file "~/.emacs.d/git-packages/weyland-yutani-theme.el"))

(defun set-synthwave-theme ()
  (load-file "~/.emacs.d/git-packages/emacs-synthwave-theme/synthwave-theme.el"))
