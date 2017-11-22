(require 'org)
(setq
    org-directory "~/Dropbox/Documents/Org/"
    org-default-notes-file (expand-file-name "refile.org" org-directory)
    org-default-journal-file (expand-file-name "journal.org" org-directory)
    org-clock-persist-file (expand-file-name "org-clock-save.el" emacs-local-dir)
    org-clock-persist 'history)
(setq org-capture-templates
      '(("t" "todo" entry (file org-default-notes-file)
	 "* TODO %?\n%U\n%a\n" :clock-in t :clock-resume t)
	("r" "respond" entry (file org-default-notes-file)
	 "* NEXT Respond to %:from on %:subject\nSCHEDULED: %t\n%U\n%a\n" :clock-in t :clock-resume t :immediate-finish t)
	("n" "note" entry (file org-default-notes-file)
	 "* %? :NOTE:\n%U\n%a\n" :clock-in t :clock-resume t)
	("j" "Journal" entry (file+datetree org-default-journal-file)
	 "* %?\n%U\n" :clock-in t :clock-resume t)
	("w" "org-protocol" entry (file org-default-notes-file)
	 "* TODO Review %c\n%U\n" :immediate-finish t)
	("m" "Meeting" entry (file org-default-notes-file)
	 "* MEETING with %? :MEETING:\n%U" :clock-in t :clock-resume t)
	("h" "Habit" entry (file org-default-notes-file)
	 "* NEXT %?\n%U\n%a\nSCHEDULED: %(format-time-string \"%<<%Y-%m-%d %a .+1d/3d>>\")\n:PROPERTIES:\n:STYLE: habit\n:REPEAT_TO_STATE: NEXT\n:END:\n")))
;; Standard key bindings
(global-set-key "\C-cl" 'org-store-link)
(global-set-key (kbd "<f12>") 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
;; I use C-c c to start capture mode
(global-set-key (kbd "C-c c") 'org-capture)


