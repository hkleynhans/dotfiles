;;; Emacs look and feel
;;


(require 'cl)


(setq inhibit-startup-message t)
(setq inhibit-startup-echo-area-message t)

(menu-bar-mode -1)
(tool-bar-mode -1)

(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))

(column-number-mode 1)		;; Show column number in mode bar
(global-font-lock-mode 1)	;; Syntax highlighting

;; (setq font-lock-maximum-decoration
;;       '((emacs-lisp-mode . t)
;; 	(c-mode . t)
;; 	(c++-mode . 1)
;; 	(t . t)))

;;; Disable beep
(setq visual-bell t)

;;; Colorize selection
(transient-mark-mode 'on)

;;; Show matching parens
(show-paren-mode t)

(setq frame-title-format (concat "%b - emacs@" system-name))

(load-theme 'wombat)

(fset 'yes-or-no-p 'y-or-n-p)

(provide 'init-ui)
