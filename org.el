(require 'org)

(defun journal-file-insert ()
  "Insert's the journal heading based on the file's name."
  (interactive)
  (when (string-match "\\(20[0-9][0-9]\\)\\([0-9][0-9]\\)\\([0-9][0-9]\\)"
                      (buffer-name))
    (let ((year  (string-to-number (match-string 1 (buffer-name))))
          (month (string-to-number (match-string 2 (buffer-name))))
          (day   (string-to-number (match-string 3 (buffer-name))))
          (datim nil))
      (setq datim (encode-time 0 0 0 day month year))
      (insert (format-time-string
                 "#+TITLE: Journal Entry- %Y-%b-%d (%A)\n#+STARTUP: showeverything" datim)))))

(defun get-journal-file-today ()
  "Return filename for today's journal entry."
  (let ((daily-name (format-time-string org-journal-file-format)))
    (expand-file-name daily-name org-journal-dir)))

;; (add-hook 'find-file-hook 'auto-insert)
;; (add-to-list 'auto-mode-alist `(,(expand-file-name org-journal-dir) . journal-file-insert))

(defun air-pop-to-org-agenda (split)
  "Visit the org agenda, in the current window or a SPLIT."
  (interactive "P")
  (org-agenda-list)
  (when (not split)
    (delete-other-windows)))

(use-package org
  :ensure t
  :config
  ;; timesheet requires auctex which is a big package
  ;;(use-package timesheet
  ;;  :ensure t
  ;;  )
  (use-package org-journal
    :ensure t
    :config

    ;; org-journal
    (setq
      org-journal-file-format "%Y%m%d.org")
    )
  ;; orgmode
  (setq
    org-todo-keywords '((sequence "TODO" "IN-PROGRESS" "WAITING" "|" "DONE" "CANCELED"))
    org-modules (append org-modules '(org-habit))
    org-startup-folded nil
    org-time-clocksum-format (quote (:hours "%d" :require-hours t :minutes ":%02d" :require-minutes t))
    org-duration-format (quote h:mm)
    org-log-done t
    org-tag-alist '(("@work" . ?w) ("@home" . ?h))
    org-default-notes-file (expand-file-name "refile.org" my-org-data-directory)
    org-agenda-files (list my-org-data-directory)
    org-default-journal-file 'get-journal-file-today
    org-clock-persist-file (expand-file-name "org-clock-save.el" my-emacs-data-dir)
    org-clock-persist 'history)

  (setq org-capture-templates
    '(
       ;;("j" "journal" entry (file (get-journal-file-today)) "* Event: %?\n  %i")
       ("j" "journal" entry (file (get-journal-file-today)) "* %(format-time-string org-journal-time-format)%i%?")
       ("t" "todo"    entry (file org-default-notes-file) "* TODO %?\n%U\n%a\n" :clock-in t :clock-resume t)
       ("r" "respond" entry (file org-default-notes-file) "* NEXT Respond to %:from on %:subject\nSCHEDULED: %t\n%U\n%a\n" :clock-in t :clock-resume t :immediate-finish t)
       ("n" "note"    entry (file org-default-notes-file) "* %? :NOTE:\n%U\n%a\n" :clock-in t :clock-resume t)
       ;;("j" "Journal" entry (file+datetree org-default-journal-file) "* %?\n%U\n" :clock-in t :clock-resume t)
       ("m" "Meeting" entry (file org-default-notes-file) "* MEETING with %? :MEETING:\n%U" :clock-in t :clock-resume t)
       ("h" "Habit"   entry (file org-default-notes-file)
         "* NEXT %?\n%U\n%a\nSCHEDULED: %(format-time-string \"%<<%Y-%m-%d %a .+1d/3d>>\")\n:PROPERTIES:\n:STYLE: habit\n:REPEAT_TO_STATE: NEXT\n:END:\n")
       ("w" "org-protocol" entry (file org-default-notes-file) "* TODO Review %c\n%U\n" :immediate-finish t)
       )
    )
  ;; Standard key bindings
  (global-set-key "\C-cl" 'org-store-link)
  ;; org-agenda
  (global-set-key (kbd "<f12>") 'org-agenda)
  (global-set-key "\C-ca" 'org-agenda)
  (global-set-key "\C-cb" 'org-iswitchb)
  ;; I use C-c c to start capture mode
  (global-set-key (kbd "C-c c") 'org-capture)
  (define-key global-map (kbd "C-c t a") 'air-pop-to-org-agenda)
  )
