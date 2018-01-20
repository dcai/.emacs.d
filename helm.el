(use-package helm
  :ensure t
  :diminish helm-mode
  :config
  (require 'helm-config)

  (setq helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
    helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
    helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
    helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
    helm-ff-file-name-history-use-recentf t
    helm-echo-input-in-header-line t)

  (define-key global-map [remap find-file] 'helm-find-files)
  (define-key global-map [remap recentf-open-files] 'helm-recentf)
  (define-key global-map [remap occur] 'helm-occur)
  (define-key global-map [remap list-buffers] 'helm-buffers-list)
  (define-key global-map [remap dabbrev-expand] 'helm-dabbrev)
  (define-key global-map [remap execute-extended-command] 'helm-M-x)

  (use-package helm-ls-git
    :ensure t)

  (use-package helm-ag
    :ensure t
    )

  (global-set-key (kbd "C-c h") 'helm-command-prefix)
  (global-unset-key (kbd "C-x c"))
  (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to run persistent action
  (define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB work in terminal
  (define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z

  (helm-mode 1)

  (unless (boundp 'completion-in-region-function)
    (define-key lisp-interaction-mode-map [remap completion-at-point] 'helm-lisp-completion-at-point)
    (define-key emacs-lisp-mode-map       [remap completion-at-point] 'helm-lisp-completion-at-point))
  (add-hook 'kill-emacs-hook #'(lambda () (and (file-exists-p "$CONF_FILE") (delete-file "$CONF_FILE"))))
  )