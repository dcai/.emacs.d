(when (display-graphic-p)
  (menu-bar-mode 1)
  (toggle-scroll-bar 1)
  (tool-bar-mode -1)
  )

(use-package gruvbox-theme
  :ensure t
  :config
  (load-theme 'gruvbox-dark-medium t)
  )

(when (eq system-type 'darwin)
  (set-face-attribute 'default nil :family "Monaco")
  (set-face-attribute 'default nil :height 165)
  )