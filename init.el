;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
;; (package-initialize)

;; emacs config
(setq
  ;; store all backup and autosave files in the tmp dir
  backup-directory-alist `((".*" . ,temporary-file-directory))
  auto-save-file-name-transforms `((".*" ,temporary-file-directory t))
  custom-file "~/.emacs.d/custom.el"
  inhibit-splash-screen t
  init-verbose t ;; message before each loaded file
  dotfiles-dir (file-name-directory (or (buffer-file-name)
                                        load-file-name))
  hosts-dir (file-name-as-directory (expand-file-name "hosts" dotfiles-dir))
  host-default-dir (file-name-as-directory (expand-file-name "default" hosts-dir))
  hostname (system-name)
  host-specific-dir (file-name-as-directory (expand-file-name hostname hosts-dir))
  ;; wait until hostname is simplified before constructing host-specific-dir
  users-dir (file-name-as-directory (expand-file-name "users" dotfiles-dir))
  user-default-dir (file-name-as-directory (expand-file-name "default" users-dir))
  user-specific-dir (file-name-as-directory (expand-file-name user-login-name users-dir)))

;; init-load function
(defun init-load (filename &optional noerror)
  "Load FILENAME and provide message when init-verbose passing in optional NOERROR."
  (when init-verbose
    (message (format ";; LOAD: %s -------------------------" filename)))
  (load filename noerror))

;; load plugin config
(init-load (expand-file-name "package.el" dotfiles-dir))
(init-load (expand-file-name "evil.el" dotfiles-dir))
(init-load (expand-file-name "org.el" dotfiles-dir))
(init-load (expand-file-name custom-file dotfiles-dir))

;; load any system and user specific files
(dolist (dir (list host-default-dir host-specific-dir user-default-dir user-specific-dir))
  (when (file-exists-p dir)
    (add-to-list 'load-path dir)
    (mapc #'load (directory-files dir nil ".*el$"))))

;; enable transient mark mode
(transient-mark-mode 1)
