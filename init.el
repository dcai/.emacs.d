;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
;; (package-initialize)

;; Some custom consts
(setq
    local-emacs-dotfiles-dir (file-name-directory (or (buffer-file-name) load-file-name))
    ;; store emacs configs which are not tracked by git
    local-emacs-data-dir "~/.local/share/emacs"
)

;; Save all tempfiles in $TMPDIR/emacs$UID/
(defconst local-emacs-tmp-dir (expand-file-name (format "emacs%d" (user-uid)) local-emacs-data-dir))

(unless (file-exists-p local-emacs-data-dir)
      (make-directory local-emacs-data-dir t))
;; UTF-8 as default encoding
(set-language-environment "UTF-8")

(defun reload-init-file ()
  (interactive)
  (load-file user-init-file))

(progn
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
  custom-file (expand-file-name "custom.el" local-emacs-data-dir)
  init-verbose t ;; message before each loaded file
  ;; scroll one line at a time (less "jumpy" than defaults)
  mouse-wheel-scroll-amount '(1 ((shift) . 1)) ;; one line at a time
  mouse-wheel-progressive-speed nil ;; don't accelerate scrolling
  mouse-wheel-follow-mouse 't ;; scroll window under mouse
  scroll-step 1 ;; keyboard scroll one line at a time
  ;; no tab
  c-basic-indent 2
  tab-width 4
  indent-tabs-mode nil
  ;; end of no tab
  make-backup-file nil
  ;; store all backup and autosave files in the tmp dir
  ;; backup-directory-alist `((".*" . ,temporary-file-directory))
  ;; auto-save-file-name-transforms `((".*" ,temporary-file-directory t))
  ;; auto-save-list-file-prefix nil
  backup-directory-alist `((".*" . , local-emacs-tmp-dir))
  auto-save-file-name-transforms `((".*", local-emacs-tmp-dir t))
  auto-save-list-file-prefix local-emacs-tmp-dir

  recentf-save-file (expand-file-name "recentf" local-emacs-data-dir)
  bookmark-default-file (expand-file-name "bookmarks" local-emacs-data-dir)
  hosts-dir (file-name-as-directory (expand-file-name "hosts" local-emacs-dotfiles-dir))
  host-default-dir (file-name-as-directory (expand-file-name "default" hosts-dir))
  hostname (system-name)
  host-specific-dir (file-name-as-directory (expand-file-name hostname hosts-dir))
  ;; wait until hostname is simplified before constructing host-specific-dir
  users-dir (file-name-as-directory (expand-file-name "users" local-emacs-dotfiles-dir))
  user-default-dir (file-name-as-directory (expand-file-name "default" users-dir))
  user-specific-dir (file-name-as-directory (expand-file-name user-login-name users-dir)))
;; init-load function
(defun init-load (filename &optional noerror)
  "Load FILENAME and provide message when init-verbose passing in optional NOERROR."
  (when init-verbose
    (message (format ";; LOAD: %s -------------------------" filename)))
  (load filename noerror))

;; create custom file is not exists
(write-region "" nil custom-file)

;; load plugin config
(init-load (expand-file-name "package.el" local-emacs-dotfiles-dir))
(init-load (expand-file-name "evil.el" local-emacs-dotfiles-dir))
(init-load (expand-file-name "org.el" local-emacs-dotfiles-dir))
;;(init-load (expand-file-name "ido.el" local-emacs-dotfiles-dir))
(init-load (expand-file-name "helm.el" local-emacs-dotfiles-dir))
(unless (eq system-type 'windows-nt)
  (init-load custom-file) ;; don't load custom-file for windows, i don't know why it fails
  )
;; load any system and user specific files
(dolist (dir (list host-default-dir host-specific-dir user-default-dir user-specific-dir))
  (when (file-exists-p dir)
    (add-to-list 'load-path dir)
    (mapc #'load (directory-files dir nil ".*el$"))))
