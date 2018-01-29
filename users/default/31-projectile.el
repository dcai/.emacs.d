;; https://github.com/bbatsov/projectile/issues/989
(setq
  ;; projectile-enable-caching t
  projectile-known-projects-file (expand-file-name "projectile-bookmarks.eld" my-emacs-data-dir)
  projectile-cache-file  (expand-file-name "projectile.cache" my-emacs-data-dir)
  )

(use-package projectile
  :ensure t
  :diminish projectile-mode
  :after helm
  :config
  (projectile-global-mode)
  (setq
    ;; projectile-generic-command "rg --files"
    ;; projectile-generic-command "ag --no-color -g"
    projectile-switch-project-action 'helm-projectile
    projectile-completion-system 'helm
    projectile-indexing-method 'alien
    )
  )

(use-package helm-projectile
  :ensure t
  :config
  (helm-projectile-on)

  ;; This depends on `helm-ag-base-command` option defined for helm-ag plugin
  ;; patch helm-projectile-ag to support rg
  ;; without this patch, helm-projectile-ag passes invalid `--ignore` arg to rg
  (defun helm-projectile-ag (&optional options)
    "Helm version of projectile-ag."
    (interactive (if current-prefix-arg (list (read-string "option: " "" 'helm-ag--extra-options-history))))
    (if (require 'helm-ag nil  'noerror)
      (if (projectile-project-p)
        (let ((helm-ag-command-option options)
               (current-prefix-arg nil))
          (helm-do-ag (projectile-project-root) (car (projectile-parse-dirconfig-file))))
        (error "You're not in a project"))
      (error "Error: helm-ag not available")))
  )

(defun my-invalidate-cache ()
  "Invalidate projectile and recentf cache."
  (interactive)
  (progn
    (projectile-invalidate-cache nil)
    (recentf-cleanup)
    )
  )