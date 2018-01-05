(require 'package)
(setq
  package-user-dir (expand-file-name "packages" my-emacs-data-dir)
  package-archives '(
                      ("gnu" . "http://elpa.gnu.org/packages/")
                      ("org" . "http://orgmode.org/elpa/")
                      ("melpa" . "http://melpa.org/packages/")))
(package-initialize)

(setq package-list '(
                      use-package
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

;;(use-package highlight-chars
;;  :config
;;  (progn
;;    (add-hook 'font-lock-mode-hook 'hc-highlight-tabs)
;;    ))
;
(use-package blank-mode
  :ensure t
  :config
  (setq
    blank-tab 'underline
    ))

(use-package editorconfig
  :ensure t
  :config
  (editorconfig-mode 1))
