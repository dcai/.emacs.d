; (require 'evil-leader)

(evil-mode 1)

(defun reload-init-file ()
  (interactive)
  (load-file user-init-file))

; (global-evil-leader-mode)
; (evil-leader/set-leader "<SPC>")
; (global-set-key (kbd "SPC") nil) ;; Remove the old keybinding
(global-set-key (kbd "C-x C-l") 'reload-init-file)
; (global-set-key (kbd "SPC f f") 'fzf)
; (global-set-key (kbd "SPC f s") 'save-buffer)

; (evil-leader/set-key
  ; "ff" 'fzf
  ; ;"feR" 'reload-init-file
  ; )
