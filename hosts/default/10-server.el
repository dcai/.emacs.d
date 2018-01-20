(require 'server)
;; https://stackoverflow.com/a/1566618/69938
(when (and (>= emacs-major-version 23)
           (equal window-system 'w32))
  (defun server-ensure-safe-dir (dir) "Noop" t)) ; Suppress error "directory
                                                 ; ~/.emacs.d/server is unsafe"
                                                 ; on windows.

(setq
  server-auth-dir (expand-file-name "server" my-emacs-data-dir)
  server-name "main-server"   ;;Server mutex file name
  )

(unless (file-exists-p server-auth-dir)
  (make-directory server-auth-dir))

;; (server-start)