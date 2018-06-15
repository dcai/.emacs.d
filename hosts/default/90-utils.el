(setq scratch-buffer-file "~/.emacs.d/scratch.el")

(setq initial-scratch-message "")           ;initial message
(add-hook 'kill-emacs-hook 'scratch-save)   ;
(add-hook 'window-setup-hook 'scratch-resume);

;;  window-setup-hook
;;  @see info 38.1.1 Summary: Sequence of Actions at Startup
(add-hook 'kill-buffer-hook; *scratch*
  (lambda ()
    (if (equal (buffer-name) "*scratch*") (scratch-save))))

(add-hook 'after-save-hook
  (lambda ()
    (unless (get-buffer "*scratch*") (scratch-resume))))


(defun scratch-save ()
  (let ((buf (get-buffer "*scratch*")))
    (when buf
      (set-buffer buf)
      (write-file scratch-buffer-file)
      (ignore-errors (kill-buffer "scratch.el")))))

(defun scratch-resume ()
  "*scratch* "
  (interactive)
  (set-buffer (get-buffer-create "*scratch*"))
  (funcall initial-major-mode)
  (insert-file-contents scratch-buffer-file nil nil nil t)

  (ignore-errors (kill-buffer ".scratch")))(defun scratch ()
                                             "
   Switches to scratch buffer and creates
   it if it doesn't exist.

   Usage: M-x scratch

   This function is useful to Elisp developers.

   Suggestion:
        Add (defalias 's #'scratch) to the init file.
        You can switch to the scratch buffer with > M-x s
   "

                                             (interactive)

                                             (let ((buf (get-buffer-create "*scratch*")))

                                               (switch-to-buffer buf)
                                               (lisp-interaction-mode)
                                               ))

(defalias 's #'scratch)

(defun js/scratch ()
  (interactive)
  (let (
         ;; Creates a new buffer object.
         (buf (get-buffer-create "*js-scratch*"))
         )
    ;; Executes functions that would change the current buffer at
    ;; buffer buf
    (with-current-buffer buf
       ;;; Set the new buffer to scratch mode
      (rjsx-mode)
       ;;; Pop to scratch buffer
      (pop-to-buffer buf)
      )))