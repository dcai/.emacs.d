(use-package evil
  :ensure t
  :config
  (evil-mode 1)

  (use-package evil-leader
    :ensure t
    :config
    (evil-leader/set-leader "<SPC>")
    (global-evil-leader-mode)
    ;; (global-set-key (kbd "SPC f f") 'fzf)
    ;; (global-set-key (kbd "SPC") nil) ;; Remove the old keybinding
    ;; (global-set-key (kbd "SPC f s") 'save-buffer)

    (evil-leader/set-key
      "." 'helm-projectile-ag
      "qq" 'save-buffers-kill-emacs
      "." 'helm-projectile-ag
      "qq" 'save-buffers-kill-emacs
      "ff" 'find-file
      "ff" 'helm-projectile-find-file
      "fr" 'recentf-open-files
      "fs" 'save-buffer
      "bd" 'kill-this-buffer
      )
    )
  (use-package evil-nerd-commenter
    :ensure t
    :after evil-leader
    :config
    (global-evil-leader-mode)
    (evil-leader/set-key
      "ci" 'evilnc-comment-or-uncomment-lines
      "cl" 'evilnc-quick-comment-or-uncomment-to-the-line
      "ll" 'evilnc-quick-comment-or-uncomment-to-the-line
      "cc" 'evilnc-copy-and-comment-lines
      "cp" 'evilnc-comment-or-uncomment-paragraphs
      "cr" 'comment-or-uncomment-region
      "cv" 'evilnc-toggle-invert-comment-line-by-line
      ;; "."  'evilnc-copy-and-comment-operator
      "\\" 'evilnc-comment-operator ; if you prefer backslash key
      )
    )

  (use-package let-alist
    :ensure t
    :config
    (use-package evil-org
      :ensure t
      :after org
      :config
      (add-hook 'org-mode-hook 'evil-org-mode)
      (add-hook 'evil-org-mode-hook
        (lambda ()
          (evil-org-set-key-theme))))
    )

  (use-package evil-matchit
    :ensure t
    :config
    (global-evil-matchit-mode 1)
    )

  (use-package evil-visualstar
    :ensure t
    :config
    (global-evil-visualstar-mode)
    )

  (use-package evil-matchit
    :ensure t
    :config
    (global-evil-matchit-mode 1)
    )

  (use-package evil-surround
    :ensure t
    :config
    (global-evil-surround-mode))

  (use-package evil-indent-textobject
    :ensure t
    )
  )