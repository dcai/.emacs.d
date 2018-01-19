;; https://github.com/bbatsov/projectile/issues/989
(setq
  ;; projectile-enable-caching t
  projectile-known-projects-file (expand-file-name "projectile-bookmarks.eld" my-emacs-data-dir)
  projectile-cache-file  (expand-file-name "projectile.cache" my-emacs-data-dir)
  )

(use-package projectile
  :ensure t
  :after helm
  :config
  (projectile-global-mode)
  (setq
    ;; projectile-generic-command "rg --files"
    projectile-generic-command "ag --no-color -g"
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

(defun my-invalidate-cache ()
  "Invalidate projectile and recentf cache."
  (interactive)
  (progn
    (projectile-invalidate-cache nil)
    (recentf-cleanup)
    )
  )