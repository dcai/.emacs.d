;; https://www.emacswiki.org/emacs/EndOfLineTips
(defun my-unix-file ()
  "Change the current buffer to utf-8 with Unix line-ends."
  (interactive)
  (set-buffer-file-coding-system 'utf-8-unix t))

(defun my-dos-file ()
  "Change the current buffer to utf-8 with DOS line-ends."
  (interactive)
  (set-buffer-file-coding-system 'utf-8-dos t))

(defun my-mac-file ()
  "Change the current buffer to utf-8 with Mac line-ends."
  (interactive)
  (set-buffer-file-coding-system 'utf-8-mac t))