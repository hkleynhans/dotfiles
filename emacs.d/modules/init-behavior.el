;;; Behavior preferences

(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

(setq-default fill-column 79)

(setq large-file-warning-threshold 100000000)

;; Remove trailing blanks on save
(add-hook 'before-save-hook 'delete-trailing-whitespace)

(setq backup-directory-alist '(("." . "~/.emacs.d/auto-saves")))
(setq backup-by-copying t)
(setq delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)

(provide 'init-behavior)
