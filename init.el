;; -*- mode: elisp -*-

(setq inhibit-splash-screen t)

;; enable transient mark mode
(transient-mark-mode 1)

(require 'org)
(require 'package)
(package-initialize)

(setq custom-file "~/.emacs.d/local.el")
(load custom-file)
