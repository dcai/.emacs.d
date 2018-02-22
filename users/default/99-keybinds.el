(global-set-key (kbd "C-x C-l") 'reload-init-file)
(global-set-key (kbd "C-l") 'list-buffers)
(global-set-key "\C-x\ \C-r" 'recentf-open-files)
(global-set-key "\C-x\ \C-b" 'eval-buffer)

(global-set-key (kbd "C-c h") 'helm-command-prefix)
(global-unset-key (kbd "C-x c"))

;; https://emacs.stackexchange.com/a/36248/18022
;; C-c d Delete buffer without leaving helm
(define-key global-map [remap find-file] 'helm-find-files)
(define-key global-map [remap recentf-open-files] 'helm-recentf)
(define-key global-map [remap occur] 'helm-occur)
(define-key global-map [remap dabbrev-expand] 'helm-dabbrev)
(define-key global-map [remap execute-extended-command] 'helm-M-x)
(define-key global-map [remap list-buffers] 'helm-mini)
;; (define-key global-map [remap list-buffers] 'helm-buffers-list)

;; rebind tab to run persistent action
;; (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
;; make TAB work in terminal
;; (define-key helm-map (kbd "C-i") 'helm-execute-persistent-action)
;; list actions using C-z
;; (define-key helm-map (kbd "C-z")  'helm-select-action)

(evil-leader/set-key
  "feR" 'reload-init-file
  "pp" 'helm-projectile-switch-project
  "qq" 'save-buffers-kill-emacs
  "fs" 'save-buffer
  "bd" 'kill-this-buffer
  "." 'helm-projectile-ag
  ;; "ff" 'find-file
  ;; "ff" 'helm-projectile
  "ff" 'helm-projectile-find-file
  "fr" 'helm-recentf
  )

;;; orgmode
;; Standard key bindings
(global-set-key "\C-cl" 'org-store-link)
;; org-agenda
(global-set-key (kbd "<f12>") 'org-agenda)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
;; I use C-c c to start capture mode
(global-set-key (kbd "C-c c") 'org-capture)
(define-key global-map (kbd "C-c t a") 'air-pop-to-org-agenda)