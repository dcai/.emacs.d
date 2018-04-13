(use-package neotree
  :delight
  :ensure t
  :config
  (setq
    neo-window-fixed-size nil
    neo-smart-open t
    projectile-switch-project-action 'neotree-projectile-action)
  (defun my-neotree-project-dir ()
    "Open NeoTree using the git root."
    (interactive)
    (let ((project-dir (projectile-project-root))
           (file-name (buffer-file-name)))
      (neotree-toggle)
      (if project-dir
        (if (neo-global--window-exists-p)
          (progn
            (neotree-dir project-dir)
            (neotree-find file-name)))
        (message "Could not find git project root."))))

  (global-set-key [f8] 'my-neotree-project-dir)
  (add-hook 'neotree-mode-hook
    (lambda ()
      (define-key evil-normal-state-local-map (kbd "TAB") 'neotree-enter)
      (define-key evil-normal-state-local-map (kbd "SPC") 'neotree-quick-look)
      (define-key evil-normal-state-local-map (kbd "q") 'neotree-hide)
      (define-key evil-normal-state-local-map (kbd "RET") 'neotree-enter)))
  )
