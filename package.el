(require 'package)
(setq
  package-user-dir (expand-file-name "packages" my-emacs-data-dir)
  package-archives '(
                      ("gnu" . "http://elpa.gnu.org/packages/")
                      ("org" . "http://orgmode.org/elpa/")
                      ("melpa" . "http://melpa.org/packages/")))
(package-initialize)

(setq package-list '(use-package))

;; fetch the list of packages available
(unless package-archive-contents
  (package-refresh-contents))

;; Install the missing packages
;; TODO refresh package contents when missing packages
(dolist (package package-list)
	(unless (package-installed-p package)
		(package-install package)))
(use-package yasnippet
  :ensure t
  :config
  (use-package yasnippet-snippets
    :ensure t
    )
  (setq yas-snippet-dirs
    (append yas-snippet-dirs
      (list (expand-file-name "snippets" my-emacs-dotfiles-dir))
      )
    )
  (yas-global-mode 1)
  (add-hook 'prog-mode-hook #'yas-minor-mode)
  )

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

(use-package xterm-color
  :ensure t
  )

(use-package company
  :ensure t
  :config
  (add-hook 'after-init-hook 'global-company-mode)
  (use-package company-tern
    :ensure t
    :config
    (add-to-list 'company-backends 'company-tern)
    )
  )

(use-package rjsx-mode
  :ensure t
  )

(use-package smex
  :ensure t
  )
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
;; This is your old M-x.
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

(use-package editorconfig
  :ensure t
  :config
  (editorconfig-mode 1))

(use-package magit
  :ensure t
  )
(use-package avy
  :ensure t
  )
(use-package emmet-mode
  :ensure t
  )
(use-package prettier-js
  :ensure t
  :config
  (add-hook 'js2-mode-hook 'prettier-js-mode)
  )