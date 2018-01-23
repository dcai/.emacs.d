;; http://www.howardism.org/Technical/Emacs/eshell-fun.html
(defun my-eshell-here ()
  "Opens up a new shell in the directory associated with the
current buffer's file. The eshell is renamed to match that
directory to make multiple eshell windows easier."
  (interactive)
  (let* ((parent (if (buffer-file-name)
                   (file-name-directory (buffer-file-name))
                   default-directory))
          (height (/ (window-total-height) 3))
          (name   (car (last (split-string parent "/" t)))))
    (split-window-vertically (- height))
    (other-window 1)
    (eshell "new")
    (rename-buffer (concat "*eshell: " name "*"))

    (insert (concat "ls"))
    (eshell-send-input)))

(defun eshell/x ()
  (insert "exit")
  (eshell-send-input)
  (delete-window))

(global-set-key (kbd "C-`") 'my-eshell-here)

(defun eshell-mode-hook-func ()
  (setq eshell-path-env (concat "/usr/local/bin:" eshell-path-env))
  (setenv "PATH" (concat "/usr/local/bin:" (getenv "PATH")))
  )

(add-hook 'eshell-mode-hook 'eshell-mode-hook-func)

(defalias 'vim 'find-file)
(defalias 'vi 'find-file)
(defalias 'edit 'find-file)
(defalias 'emacs 'find-file)