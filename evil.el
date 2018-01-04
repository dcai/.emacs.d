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
      "qq" 'save-buffers-kill-emacs
      "ff" 'find-file
      "fr" 'recentf-open-files
      "fs" 'save-buffer
      "bd" 'kill-this-buffer
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

  (use-package evil-surround
    :ensure t
    :config
    (global-evil-surround-mode))

  (use-package evil-indent-textobject
    :ensure t))