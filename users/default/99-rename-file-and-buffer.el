;; https://stackoverflow.com/a/1834038/69938
;; Originally from stevey, adapted to support moving to a new directory.
(defun my-rename-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive
    (progn
      (if (not (buffer-file-name))
        (error "Buffer '%s' is not visiting a file!" (buffer-name)))
      ;; Disable ido auto merge since it too frequently jumps back to the original
      ;; file name if you pause while typing. Reenable with C-z C-z in the prompt.
      (let ((ido-auto-merge-work-directories-length -1))
        (list (read-file-name (format "Rename %s to: " (file-name-nondirectory
                                                         (buffer-file-name))))))))
  (if (equal new-name "")
    (error "Aborted rename"))
  (setq new-name (if (file-directory-p new-name)
                   (expand-file-name (file-name-nondirectory
                                       (buffer-file-name))
                     new-name)
                   (expand-file-name new-name)))
  ;; Only rename if the file was saved before. Update the
  ;; buffer name and visited file in all cases.
  (if (file-exists-p (buffer-file-name))
    (rename-file (buffer-file-name) new-name 1))
  (let ((was-modified (buffer-modified-p)))
    ;; This also renames the buffer, and works with uniquify
    (set-visited-file-name new-name)
    (if was-modified
      (save-buffer)
      ;; Clear buffer-modified flag caused by set-visited-file-name
      (set-buffer-modified-p nil)))

  (setq default-directory (file-name-directory new-name))

  (message "Renamed to %s." new-name))