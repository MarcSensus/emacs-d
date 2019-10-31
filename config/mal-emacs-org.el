(use-package
  org
  :ensure t
  :mode ("\\.org\\'" . org-mode)
  :bind (("C-c l" . org-store-link)
		 ("C-c c" . org-capture)
		 ("C-c a" . org-agenda)
		 ("C-c b" . org-iswitchb)
		 ("C-c C-w" . org-refile)
		 ("C-c j" . org-clock-goto)
		 ("C-c C-x C-o" . org-clock-out))
  :config (setq org-default-notes-file (concat org-directory "/notes.org"))
  (setq org-startup-indented t)	  ; Enable `org-indent-mode' by default
  ;;(add-hook 'org-mode-hook #'visual-line-mode)
  (setq org-todo-keywords (quote ((sequence "TODO(t)" "WORKING(i)" "|" "DONE(d)")
								  (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)"
											"PHONE" "MEETING"))))
  (setq org-todo-keyword-faces (quote (("TODO" :foreground "red"
										:weight bold)
									   ("WORKING" :foreground "light blue"
										:weight bold)
									   ("DONE" :foreground "forest green"
										:weight bold)
									   ("WAITING" :foreground "orange"
										:weight bold)
									   ("HOLD" :foreground "magenta"
										:weight bold)
									   ("CANCELLED" :foreground "forest green"
										:weight bold)
									   ("MEETING" :foreground "forest green"
										:weight bold)
									   ("PHONE" :foreground "forest green"
										:weight bold))))
  (setq org-todo-state-tags-triggers (quote (("CANCELLED" ("CANCELLED" . t))
											 ("WAITING" ("WAITING" . t))
											 ("HOLD" ("WAITING")
											  ("HOLD" . t))
											 (done ("WAITING")
												   ("HOLD"))
											 ("TODO" ("WAITING")
											  ("CANCELLED")
											  ("HOLD"))
											 ("WORKING" ("WAITING")
											  ("CANCELLED")
											  ("HOLD"))
											 ("DONE" ("WAITING")
											  ("CANCELLED")
											  ("HOLD")))))
  (setq org-use-fast-todo-selection t))

;; org-mw-export-as-mediawiki
(use-package
  ox-mediawiki
  :ensure t)

;; org-minutes-export-as-ascii or org-minutes-export-to-ascii
(use-package
  ox-minutes
  :ensure t)

;; org-pandoc-export-as-*
;(use-package
;  ox-pandoc
;  :ensure t)

(use-package
  org-chef
  :ensure t)

(use-package
  org-jira
  :ensure t
  :init (setq jiralib-url "https://xyleminc.atlassian.net"))

(use-package
  org-trello
  :ensure t
  :init (custom-set-variables '(org-trello-files '("~/org/trello.org" "~/org/trello-2018.org"))))

;; Capture Templates
(setq org-capture-templates '(("c" "Cookbook" entry (file "~/org/cookbook.org")
							   "%(org-chef-get-recipe-from-url)"
							   :empty-lines 1)
							  ("t" "Todo" entry (file+headline "~/org/gtd.org" "Tasks")
							   "* TODO %?\n  %i\n  %a")
							  ("j" "Journal" entry (file+olp+datetree "~/org/journal.org")
							   "* %?\nEntered on %U\n  %i\n  %a")))

(defun reflash-indentation ()
  "Fix org-indent issues, center line."
  (interactive)
  (org-indent-mode 1)
  (recenter-top-bottom))
