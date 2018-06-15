(setq
  flycheck-temp-prefix (file-name-as-directory (expand-file-name "flycheck_temp" my-emacs-tmp-dir))
  )

(use-package flycheck
  :delight flycheck-mode
  :ensure t
  :config
  (global-flycheck-mode)
  (add-hook 'after-init-hook #'global-flycheck-mode)
  (use-package flycheck-elm
    :ensure t
    )

  ;; disable jshint since we prefer eslint checking
  (setq-default flycheck-disabled-checkers
    (append flycheck-disabled-checkers
      '(javascript-jshint)))

  ;; use eslint with web-mode for jsx files
  (flycheck-add-mode 'javascript-eslint 'rjsx-mode)
  (flycheck-add-mode 'javascript-eslint 'js2-mode)
  ;; use local eslint from node_modules before global
  ;; http://emacs.stackexchange.com/questions/21205/flycheck-with-file-relative-eslint-executable
  (defun my/use-eslint-from-node-modules ()
    (let* ((root (locate-dominating-file
                   (or (buffer-file-name) default-directory)
                   "node_modules"))
            (eslint (and root
                      (expand-file-name "node_modules/eslint/bin/eslint.js"
                        root))))
      (when (and eslint (file-executable-p eslint))
        (setq-local flycheck-javascript-eslint-executable eslint))))
  (add-hook 'flycheck-mode-hook #'my/use-eslint-from-node-modules)
  )