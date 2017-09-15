;; Emacs configuration file                             -*- mode: lisp -*-

;; Use this file for HTTP proxy settings if needed, for packages.
(when (file-exists-p "~/.emacs.d/init-local-prolog.el")
  (load "~/.emacs.d/init-local-prolog.el"))

(package-initialize)

(setq package-enable-at-startup nil)
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(add-to-list 'exec-path "/usr/local/bin")

(when (eq system-type 'darwin)
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier nil))

;; Keep lisp customizations in a separate file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'noerror)

(require 'init-elpa)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(require 'init-evil)
(require 'init-flycheck)
(require 'init-helm)
;;(require 'init-powerline)
(require 'init-programming)
(require 'init-spell)

;; Essential settings
(setq inhibit-splash-screen t
      inhibit-startup-message t
      inhibit-startup-echo-area-message t)
(menu-bar-mode -1)
(tool-bar-mode -1)
(when (boundp 'scroll-bar-mode)
  (scroll-bar-mode -1))
(show-paren-mode 1)

;; Themes
(use-package sublime-themes :ensure t)
(use-package zenburn-theme :ensure t)
(use-package gruvbox-theme :ensure t)
(load-theme 'gruvbox)

(use-package yasnippet
  :ensure t
  :defer t
  :config
  (yas-reload-all)
  (setq yas-snippet-dirs '("~/.emacs.d/snippets"))
  (setq tab-always-indent 'complete)
  (setq yas-prompt-functions '(yas-completing-prompt
			       yas-ido-prompt
			       yas-dropdown-prompt))
  (define-key yas-minor-mode-map (kbd "<escape>") 'yas-exit-snippet))

(use-package projectile
  :ensure t
  :diminish projectile-mode
  :config
  (projectile-global-mode)
  (setq projectile-completion-system 'helm)
  (setq projectile-enable-caching t))


(use-package magit :ensure t :defer t)

;;; Emacs Lisp mode:
(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (yas-minor-mode t)
            (eldoc-mode)
            (highlight-symbol-mode)
            (define-key emacs-lisp-mode-map (kbd "<C-return>") 'eval-last-sexp)))

(defalias 'yes-or-no-p 'y-or-n-p)

(defvar backup-dir "~/.emacs.d/backups/")
(setq backup-directory-alist (list (cons "." backup-dir)))
(setq make-backup-files nil)
