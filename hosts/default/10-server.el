(require 'server)
;; https://stackoverflow.com/a/1566618/69938
(when (and (>= emacs-major-version 23)
           (equal window-system 'w32))
  (defun server-ensure-safe-dir (dir) "Noop" t)) ; Suppress error "directory
                                                 ; ~/.emacs.d/server is unsafe"
                                                 ; on windows.

(setq
  server-auth-dir (my-expand-dir "server/" my-emacs-data-dir)
  server-name "main-server"   ;;Server mutex file name
  )

(my-mkdir server-auth-dir)
;; (server-start)