(when (display-graphic-p)
  (menu-bar-mode 1)
  (toggle-scroll-bar 1)
  (tool-bar-mode -1)
  (set-face-attribute 'default nil :height 165)

  (when (eq system-type 'darwin)
    (set-face-attribute 'default nil :family "Monaco")
    )

  (when (eq system-type 'windows-nt)
    (set-face-attribute 'default nil :family "Consolas")
    )
  )

(use-package gruvbox-theme
  :ensure t
  :config
  (load-theme 'gruvbox-dark-medium t)
  )