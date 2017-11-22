(require 'org)
;(require 'preview)

;; Enable clock persistence
(setq
  org-clock-persist-file (expand-file-name "org-clock-save.el" emacs-local-dir)
  org-clock-persist 'history)
