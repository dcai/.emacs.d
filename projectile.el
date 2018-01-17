;; https://github.com/bbatsov/projectile/issues/989
(setq projectile-known-projects-file (expand-file-name "projectile-bookmarks.eld" my-emacs-data-dir))

(use-package projectile
  :ensure t
  :after helm
  :config
  (projectile-global-mode)
  (setq
    projectile-generic-command "ag -g"
    projectile-switch-project-action 'helm-projectile
    projectile-completion-system 'helm
    projectile-indexing-method 'alien
    )
  )

(use-package helm-projectile
  :ensure t
  :config
  (helm-projectile-on)
  )