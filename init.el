;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
;; (package-initialize)

(setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))
(setq exec-path (append exec-path '("/usr/local/bin")))

;; (defconst my-emacs-dotfiles-dir (file-name-directory (or (buffer-file-name) load-file-name)))
(defconst my-emacs-dotfiles-dir "~/.emacs.d" "Emacs config home.")
(defconst my-emacs-data-dir "~/.local/share/emacs" "Store Emacs configs which are not tracked by git.")
(defconst my-emacs-tmp-dir (expand-file-name (format "emacs%d" (user-uid)) my-emacs-data-dir))
(defconst my-init-verbose t "Message before each loaded file.")
(defconst my-hostname (system-name))
(defconst my-hosts-dir (file-name-as-directory (expand-file-name "hosts" my-emacs-dotfiles-dir)))
(defconst my-users-dir (file-name-as-directory (expand-file-name "users" my-emacs-dotfiles-dir)))
(defconst my-host-default-dir (file-name-as-directory (expand-file-name "default" my-hosts-dir)))
(defconst my-host-specific-dir (file-name-as-directory (expand-file-name my-hostname my-hosts-dir)))
(defconst my-user-default-dir (file-name-as-directory (expand-file-name "default" my-users-dir)))
(defconst my-user-specific-dir (file-name-as-directory (expand-file-name user-login-name my-users-dir)))

;; https://www.emacswiki.org/emacs/ReformatBuffer
(defun my-reformat-buffer ()
  "Reformat current buffer."
  (interactive)
  (save-excursion
    (indent-region (point-min) (point-max) nil)))

(defun reload-init-file ()
  "Reload Emacs init.el."
  (interactive)
  (load-file user-init-file))

(defun my-mkdir(dir)
  (unless (file-exists-p dir)
    (make-directory dir t))
  )

;; https://www.emacswiki.org/emacs/ReformatBuffer
(defun my-format-buffer ()
  "Format entire buffer."
  (interactive)
  (save-excursion
    (indent-region (point-min) (point-max) nil)))


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

(defvar best-gc-cons-threshold 4000000 "Best default gc threshold value.  Should't be too big.")
;; don't GC during startup to save time
(setq gc-cons-threshold most-positive-fixnum)

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
;; (add-to-list 'recentf-exclude (expand-file-name my-emacs-data-dir))
(add-to-list 'recentf-exclude "emacs/packages")
;; end of open recent files

;; create data dir if not exists
(my-mkdir my-emacs-data-dir)

;; create custom file is not exists
(unless (file-exists-p custom-file)
  (write-region "" nil custom-file)
  )

;; UTF-8 as default encoding
(set-language-environment "UTF-8")
(prefer-coding-system 'utf-8)
(show-paren-mode 1)

(defun my-load-dir(dir)
  "Load .el files from a directory"
  (interactive)
  (when (file-exists-p dir)
    (add-to-list 'load-path dir)
    (mapc #'load (directory-files dir nil ".*el$")))
  )


;; maybe try this:
;; https://gist.github.com/dotemacs/1427240
(defun my-add-to-mode (mode &rest files)
  "Map MODE to FILES."
  (dolist (file files)
    (add-to-list 'auto-mode-alist
      (cons file mode))))

;; load any site specific files before user config
;; because it may contains proxy settings
(dolist (dir (list
               my-host-default-dir
               my-host-specific-dir
               my-user-default-dir
               my-user-specific-dir
               )
          )
  (my-load-dir dir)
  )
;; load plugin config
;; (init-load (expand-file-name "ido.el" my-emacs-dotfiles-dir))
