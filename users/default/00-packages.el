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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;   Start to load other plugins
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package s
  :ensure t
  )

(use-package dash
  :ensure t
  )

(use-package emmet-mode
  :ensure t
  :init
  (add-hook 'css-mode-hook 'emmet-mode)
  (add-hook 'sgml-mode-hook 'emmet-mode)
  :config
  (setq-default emmet-move-cursor-between-quote t)
  (unbind-key "<C-return>" emmet-mode-keymap)
  (unbind-key "C-M-<left>" emmet-mode-keymap)
  (unbind-key "C-M-<right>" emmet-mode-keymap)
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
  (use-package yasnippet-snippets
    :ensure t
    )
  (yas-reload-all)
  (unbind-key "TAB" yas-minor-mode-map)
  (unbind-key "<tab>" yas-minor-mode-map)
  (setq yas-snippet-dirs
    (append yas-snippet-dirs
      (list (expand-file-name "snippets" my-emacs-dotfiles-dir))
      )
    )

  )

;; End of Install the missing packages

;;(use-package highlight-chars
;;  :config
;;  (progn
;;    (add-hook 'font-lock-mode-hook 'hc-highlight-tabs)
;;    ))

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
  :diminish company-mode
  :config
  (add-hook 'after-init-hook 'global-company-mode)
  (use-package company-tern
    :ensure t
    :config
    (add-to-list 'company-backends 'company-tern)
    )
  )

(use-package smex
  :ensure t
  :config
  (global-set-key (kbd "M-x") 'smex)
  (global-set-key (kbd "M-X") 'smex-major-mode-commands)
  ;; This is your old M-x.
  (global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)
  (setq
    smex-save-file (expand-file-name "smex-items" my-emacs-data-dir)
    )
  )

(use-package editorconfig
  :ensure t
  :diminish editorconfig-mode
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
  (add-to-list 'auto-mode-alist '("\\.babelrc\\'" . json-mode))
  )

(use-package js2-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.js\\'" . rjsx-mode))
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
  :ensure t
  :config
  (add-hook 'js2-mode-hook #'js2-refactor-mode)
  (js2r-add-keybindings-with-prefix "C-c C-m")
  )

(use-package prettier-js
  :ensure t
  :config
  (add-hook 'js2-mode-hook 'prettier-js-mode)
  (add-hook 'json-mode-hook 'prettier-js-mode)
  )

(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook)
  (setq
    dashboard-items '(
                       (recents . 5)
                       (projects . 10)
                       (bookmarks . 5)
                       )
    )

  )

(use-package diminish
  :ensure t
  :config
  (diminish 'emacs-lisp-mode)
  (diminish 'undo-tree-mode)
  (diminish 'auto-revert-mode)
  (diminish 'flycheck-mode)
  (diminish 'evil-mc-mode)
  )

(use-package shell-switcher
  :ensure t
  :config
  (shell-switcher-mode)
  )