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
;; End of Install the missing packages

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;   Start to load other plugins
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package delight
  :ensure t
  )

(use-package autorevert
  :delight auto-revert-mode)

(use-package undo-tree
  :delight undo-tree-mode)

(use-package s
  :ensure t
  )

(use-package dash
  :ensure t
  )

(use-package dashboard
   :delight
   :ensure t
   :config
   (dashboard-setup-startup-hook)
   (setq
     dashboard-items '(
         (projects . 10)
         (recents . 5)
         (bookmarks . 5)
       )
     )
   )

(use-package hippie-exp
  :ensure nil
  :defer t
  :bind ("<C-tab>" . hippie-expand)
  :config
  (setq-default hippie-expand-try-functions-list
    '(yas-hippie-try-expand emmet-expand-line)))

(use-package yasnippet
  :ensure t
  :init
  (add-hook 'js-mode-hook 'yas-minor-mode)
  (add-hook 'sgml-mode-hook 'yas-minor-mode)

  :config
  (setq yas-snippet-dirs
    (append yas-snippet-dirs
      (list (expand-file-name "snippets" my-emacs-dotfiles-dir))
      )
    )
  (use-package yasnippet-snippets
    :ensure t
    )
  (yas-reload-all)
  (unbind-key "TAB" yas-minor-mode-map)
  (unbind-key "<tab>" yas-minor-mode-map)
  )

(use-package company
  :ensure t
  :delight company-mode
  :config
  (add-hook 'after-init-hook 'global-company-mode)
  )

(use-package editorconfig
  :ensure t
  :delight editorconfig-mode
  :config
  (editorconfig-mode 1))

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
          ("\\.md\\'" . markdown-mode)
          ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

(use-package magit
  :ensure t
  :config
  ;; WORKAROUND https://github.com/magit/magit/issues/2395
  (define-derived-mode magit-staging-mode magit-status-mode "Magit staging"
    "Mode for showing staged and unstaged changes."
    :group 'magit-status)
  (defun magit-staging-refresh-buffer ()
    (magit-insert-section (status)
      (magit-insert-untracked-files)
      (magit-insert-unstaged-changes)
      (magit-insert-staged-changes)))
  (defun magit-staging ()
    (interactive)
    (magit-mode-setup #'magit-staging-mode))
  )

(use-package avy
  :ensure t
  :config
  (global-set-key (kbd "C-:") 'avy-goto-char)
  )

(use-package json-mode
  :ensure t
  :config
  (my-add-to-mode 'json-mode
    "\\.babelrc$"
    "\\.eslintrc$"
    "\\.prettierrc$"
    "\\.tern-project$"
    )
  )

(use-package js2-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.js\\'" . rjsx-mode))
  (setq-default js2-strict-trailing-comma-warning nil)
  )

(use-package rjsx-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.js\\'" . rjsx-mode))
  )

(use-package multiple-cursors
  :ensure t
  )

(use-package js2-refactor
  :delight
  :ensure t
  :config
  (add-hook 'js2-mode-hook #'js2-refactor-mode)
  (js2r-add-keybindings-with-prefix "C-c C-j")
  )

(use-package prettier-js
  :delight
  :ensure t
  :init
  (add-hook 'js2-mode-hook 'prettier-js-mode)
  (add-hook 'json-mode-hook 'prettier-js-mode)
  )

(use-package smartparens
  :ensure t
  :delight
  :config
  (add-hook 'prog-mode-hook #'smartparens-mode)
  (add-hook 'rjsx-mode-hook #'smartparens-mode)
  )

(use-package yaml-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
  (add-to-list 'auto-mode-alist '("\\.yaml\\'" . yaml-mode))

  (add-hook 'yaml-mode-hook
    '(lambda ()
       (define-key yaml-mode-map "\C-m" 'newline-and-indent)))
  )
;;(use-package highlight-chars
;;  :config
;;  (progn
;;    (add-hook 'font-lock-mode-hook 'hc-highlight-tabs)
;;    ))
;; (use-package blank-mode
;;   :ensure t
;;   :init
;;   (setq
;;     blank-tab 'underline
;;     ))
;; (use-package xterm-color
;;   :ensure t
;;   )
;; (use-package smex
;;   :ensure t
;;   :config
;;   (global-set-key (kbd "M-x") 'smex)
;;   (global-set-key (kbd "M-X") 'smex-major-mode-commands)
;;   ;; This is your old M-x.
;;   (global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)
;;   (setq
;;     smex-save-file (expand-file-name "smex-items" my-emacs-data-dir)
;;     )
;;   )
;; (use-package lispy
;;   :ensure t
;;   :config
;;   (add-hook 'emacs-lisp-mode-hook (lambda () (lispy-mode 1)))
;;   (use-package evil-lispy
;;     :ensure t
;;     :config
;;     (add-hook 'emacs-lisp-mode-hook #'evil-lispy-mode)
;;     (add-hook 'clojure-mode-hook #'evil-lispy-mode)
;;     )
;;   )
