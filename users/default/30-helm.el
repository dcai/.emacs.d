(use-package helm
  :ensure t
  :diminish helm-mode
  :config

  (helm-mode 1)

  (setq
    helm-split-window-inside-p            t ; open helm buffer inside current window, not occupy whole other window
    helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
    helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
    helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
    helm-ff-file-name-history-use-recentf t
    helm-echo-input-in-header-line t)

  (unless (boundp 'completion-in-region-function)
    (define-key lisp-interaction-mode-map [remap completion-at-point] 'helm-lisp-completion-at-point)
    (define-key emacs-lisp-mode-map       [remap completion-at-point] 'helm-lisp-completion-at-point))
  (add-hook 'kill-emacs-hook #'(lambda () (and (file-exists-p "$CONF_FILE") (delete-file "$CONF_FILE"))))

  (use-package helm-flx
    :ensure t
    :config
    (helm-flx-mode +1)
    (setq
      helm-flx-for-helm-find-files t ;; t by default
      helm-flx-for-helm-locate t ;; nil by default
      )
    )

  (use-package helm-fuzzier
    :ensure t
    :config
    (helm-fuzzier-mode 1)
    )

  (use-package helm-ls-git
    :ensure t)

  (use-package helm-descbinds
    :ensure t
    :init
    (helm-descbinds-mode)
    )

  (use-package helm-ag
    :ensure t
    :config
    (custom-set-variables
      (cond
        ((executable-find "rg")
          '(helm-ag-base-command "rg --vimgrep --ignore-case")
          )
        ((executable-find "ag")
          '(helm-ag-base-command "ag --vimgrep --ignore-case")
          )
        )

      '(helm-ag-command-option "--all-text")
      '(helm-ag-insert-at-point 'symbol)
      )
    )
  )