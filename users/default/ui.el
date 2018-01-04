(when (display-graphic-p)
  (progn
    (menu-bar-mode 1)
    ;;(toggle-scroll-bar -1)
    (tool-bar-mode -1)
    )
  )

(use-package gruvbox-theme
  :ensure t
  :config
  (load-theme 'gruvbox t)
  )
