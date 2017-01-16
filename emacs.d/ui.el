;; User interface configuration                         -*- mode: lisp -*-

;; Disable some GUI features
(when (featurep 'menu-bar) (menu-bar-mode -1))
(when (featurep 'tool-bar) (tool-bar-mode -1))
(when (featurep 'tooltip) (tooltip-mode -1))
(when (featurep 'scroll-bar) (scroll-bar-mode -1))

(setq inhibit-splash-screen t)

;; syntax coloring
(global-font-lock-mode 1)
(load-theme 'wombat t)

(blink-cursor-mode -1)
(setq visible-bell t)
(fset 'yes-or-no-p 'y-or-n-p)

(transient-mark-mode t)                      ;; will highlight region between point and mark.
(setq search-highlight t)                    ;; incremental search highlights
(setq query-replace-highlight t)             ;; highlight during query
(setq show-parent-mode t)
 
;; basic indentation
(setq-default tab-width 8)
(setq-default indent-tabs-mode nil)
(setq require-final-newline t)

(setq-default line-number-mode t) 
(setq-default column-number-mode t) 

;; IDO mode
(ido-mode 1)
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
