;; Emacs configuration file                             -*- mode: lisp -*-

;; Use this file for HTTP proxy settings if needed, for packages.
(when (file-exists-p "~/.emacs.d/init-local-prolog.el")
  (load "~/.emacs.d/init-local-prolog.el"))

(setq inhibit-startup-screen t
      initial-scratch-message ";; ready\n\n"
      package-user-dir "~/.emacs-elpa"
      package-archives '(("melpa" . "http://melpa.milkbox.net/packages/")
                         ("gnu" . "http://elpa.gnu.org/packages/")))
(eval-when-compile
  (require 'package)
  (package-initialize)
  (unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package))
  (require 'use-package)
  (setq use-package-always-ensure t))

(when (eq system-type 'darwin)
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier nil)
  (setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))
  (toggle-frame-fullscreen))

(add-to-list 'exec-path "/usr/local/bin")


;; Essential settings
(setq inhibit-splash-screen t
      inhibit-startup-message t
      inhibit-startup-echo-area-message t)
(menu-bar-mode -1)
(tool-bar-mode -1)
(when (boundp 'scroll-bar-mode)
  (scroll-bar-mode -1))
(show-paren-mode 1)

(use-package gruvbox-theme
  :config
  (load-theme 'gruvbox t))

(split-window-right)

(use-package ag
  :config
  (setq ag-reuse-buffers t)
  (setq ag-highlight-search t))

;; Keep lisp customizations in a separate file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'noerror)

;; -------------------------------------------------
;; 
;; -------------------------------------------------

;;(use-package ivy
;;  :config
;;  (ivy-mode 1)
;;  (setq ivy-use-virtual-buffers t)
;;  (setq ivy-height 10)
;;  )

(require 'ido)
(ido-mode)
(ido-everywhere)

;; -------------------------------------------------
;; Evil Mode
;; -------------------------------------------------

(use-package evil
  :config
  (define-key evil-normal-state-map (kbd "] q") 'next-error)
  (define-key evil-normal-state-map (kbd "[ q") 'previous-error)
  (setq evil-want-C-u-scroll t)
  (evil-mode 1)

  (use-package evil-leader
    :config
    (global-evil-leader-mode)
    (progn
      (setq evil-leader/in-all-states t)
      (evil-leader/set-key
	"a"    'ag-project
	"A"    'ag
	)))


  ;;(use-package evil-jumper
  ;;  :config
  ;;  (global-evil-jumper-mode))

  (use-package evil-surround
    :config
    (global-evil-surround-mode))

  (use-package evil-indent-textobject))
