(require 'package)
(setq package-user-dir "~/.local/emacs/packages")
(setq package-archives '(
        ("gnu" . "http://elpa.gnu.org/packages/")
        ("org" . "http://orgmode.org/elpa/")
        ("melpa" . "http://melpa.org/packages/")
        ("melpa-stable" . "http://stable.melpa.org/packages/")
    )
)
(package-initialize)
; (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
; (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/") t)
; (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
; (add-to-list 'package-archives '("marmalade" . "https://marmalade-repo.org/packages/"))
; (add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)

(setq package-list '(
        evil
        ;;evil-leader
        fzf
        org
        ;;timesheet
        ;;auctex
    )
)

;; fetch the list of packages available
(unless package-archive-contents
  (package-refresh-contents))

;; Install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))
;; End of Install the missing packages
