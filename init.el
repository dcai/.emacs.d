;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
;; (package-initialize)

;; (defconst my-emacs-dotfiles-dir (file-name-directory (or (buffer-file-name) load-file-name)))
(defconst my-emacs-dotfiles-dir "~/.emacs.d" "emacs config home")
(defconst my-emacs-data-dir "~/.local/share/emacs" "store emacs configs which are not tracked by git")
(defconst my-emacs-tmp-dir (expand-file-name (format "emacs%d" (user-uid)) my-emacs-data-dir))
(defconst my-init-verbose t "message before each loaded file")
(defconst my-hostname (system-name))
(defconst my-hosts-dir (file-name-as-directory (expand-file-name "hosts" my-emacs-dotfiles-dir)))
(defconst my-users-dir (file-name-as-directory (expand-file-name "users" my-emacs-dotfiles-dir)))
(defconst my-host-default-dir (file-name-as-directory (expand-file-name "default" my-hosts-dir)))
(defconst my-host-specific-dir (file-name-as-directory (expand-file-name my-hostname my-hosts-dir)))
(defconst my-user-default-dir (file-name-as-directory (expand-file-name "default" my-users-dir)))
(defconst my-user-specific-dir (file-name-as-directory (expand-file-name user-login-name my-users-dir)))

(defun reload-init-file ()
  (interactive)
  (load-file user-init-file))

;; init-load function
(defun init-load (filename &optional noerror)
  "Load FILENAME and provide message when my-init-verbose passing in optional NOERROR."
  (when my-init-verbose
    (message (format "=> init-load: %s <=" filename)))
  (load filename noerror))

(progn
  ;; `M-x whitespace-mode` to toggle
  ;;
  ;; Make whitespace-mode with very basic background coloring for whitespaces.
  ;; http://ergoemacs.org/emacs/whitespace-mode.html
  (setq whitespace-style (quote (face spaces tabs newline space-mark tab-mark newline-mark )))
  ;; Make whitespace-mode and whitespace-newline-mode use “¶” for end of line char and “▷” for tab.
  (setq whitespace-display-mappings
    ;; all numbers are unicode codepoint in decimal. e.g. (insert-char 182 1)
    '(
       (space-mark 32 [183] [46]) ; SPACE 32 「 」, 183 MIDDLE DOT 「·」, 46 FULL STOP 「.」
       (newline-mark 10 [182 10]) ; LINE FEED,
       (tab-mark 9 [9655 9] [92 9]) ; tab
       )))

;; emacs config
(setq
  url-cache-directory (expand-file-name "url-cache/" my-emacs-data-dir)
  custom-file (expand-file-name "custom.el" my-emacs-data-dir)
  ;; scroll one line at a time (less "jumpy" than defaults)
  mouse-wheel-scroll-amount '(1 ((shift) . 1)) ;; one line at a time
  mouse-wheel-progressive-speed nil ;; don't accelerate scrolling
  mouse-wheel-follow-mouse 't ;; scroll window under mouse
  scroll-step 1 ;; keyboard scroll one line at a time
  ;; begin: no tab
  c-basic-indent 2
  tab-width 4
  indent-tabs-mode nil
  ;; end: no tab
  column-number-mode t
  make-backup-file nil
  ;; store all backup and autosave files in the tmp dir
  backup-directory-alist `((".*" . , my-emacs-tmp-dir))
  auto-save-file-name-transforms `((".*", my-emacs-tmp-dir t))
  auto-save-list-file-prefix my-emacs-tmp-dir
  nsm-settings-file (expand-file-name "network-security.data" my-emacs-data-dir)
  recentf-save-file (expand-file-name "recentf" my-emacs-data-dir)
  recentf-max-menu-items 200
  bookmark-default-file (expand-file-name "bookmarks" my-emacs-data-dir)
  )

;; open recent files
(recentf-mode 1)
(add-to-list 'recentf-exclude my-emacs-data-dir)
;; end of open recent files

;; create data dir if not exists
(unless (file-exists-p my-emacs-data-dir)
  (make-directory my-emacs-data-dir t))

;; create custom file is not exists
(unless (file-exists-p custom-file)
  (write-region "" nil custom-file)
  )

;(load custom-file)

(global-set-key (kbd "C-x C-l") 'reload-init-file)
(global-set-key (kbd "C-l") 'list-buffers)
(global-set-key "\C-x\ \C-r" 'recentf-open-files)

;; UTF-8 as default encoding
(set-language-environment "UTF-8")
(show-paren-mode 1)

;; load any site specific files before user config
;; because it may contains proxy settings
(dolist (dir (list
               my-host-default-dir
               my-host-specific-dir
               )
          )
  (when (file-exists-p dir)
    (add-to-list 'load-path dir)
    (when my-init-verbose
      (message (format "=> init-dir %s <=" dir))
      )
    (mapc #'load (directory-files dir nil ".*el$"))))

;; load any user specific files
(dolist (dir (list
               my-user-default-dir
               my-user-specific-dir
               )
          )
  (when (file-exists-p dir)
    (add-to-list 'load-path dir)
    (when my-init-verbose
      (message (format "=> init-dir %s <=" dir))
      )
    (mapc #'load (directory-files dir nil ".*el$"))))

;; load plugin config
;; (init-load (expand-file-name "evil.el" my-emacs-dotfiles-dir))
;; (init-load (expand-file-name "org.el" my-emacs-dotfiles-dir))
;; (init-load (expand-file-name "ido.el" my-emacs-dotfiles-dir))
;; (init-load (expand-file-name "helm.el" my-emacs-dotfiles-dir))
;; (init-load (expand-file-name "projectile.el" my-emacs-dotfiles-dir))
