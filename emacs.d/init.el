;; Emacs configuration file                             -*- mode: lisp -*-

;; Use this file for HTTP proxy settings if needed, for packages.
(when (file-exists-p "~/.emacs.d/init-local-prolog.el")
  (load "~/.emacs.d/init-local-prolog.el"))

(package-initialize)

(setq package-enable-at-startup nil)
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

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

(use-package projectile
  :ensure t
  :diminish projectile-mode
  :config
  (projectile-global-mode)
  (setq projectile-enable-caching t))

(use-package helm-core
  :ensure t)
(use-package helm
  :ensure t
  :config
  (evil-leader/set-key "f" 'helm-find-files)
  (evil-leader/set-key "b" 'helm-buffer-list)
  (setq helm-buffers-fuzzy-matching t)
  (helm-mode 1))

(use-package helm-swoop
  :ensure t
  :config
  (evil-leader/set-key "s" 'helm-swoop))
  
(use-package helm-projectile
  :ensure t
  :config
  (helm-projectile-on))

(use-package magit :ensure t :defer t)

;;; Emacs Lisp mode:
(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (yas-minor-mode t)
            (eldoc-mode)
            (highlight-symbol-mode)
            (define-key emacs-lisp-mode-map (kbd "<C-return>") 'eval-last-sexp)))

;; Load the packages we want
;;(let ((required-packages '(auto-complete
;;			   auto-complete-c-headers
;;			   evil-mode
;;			   flycheck
;;			   ggtags
;;			   company
;;			   rtags))
;;     (has-refreshed nil))
;;  (dolist (p required-packages)
;;    (unless (package-installed-p p)
;;      (unless has-refreshed
;;        (message "Refreshing package database...")
;;        (package-refresh-contents)
;;        (setq has-refreshed t)
;;        (message "Done."))
;;      (package-install p))))


;;(require 'rtags)
;;(require 'company-rtags)

;;(evil-mode 1)

;;(setq rtags-completions-enabled t)
;;(eval-after-load 'company
;;  '(add-to-list
;;    'company-backends 'company-rtags))
;;(setq rtags-autostart-diagnostics t)
;;(rtags-enable-standard-keybindings)

;(require 'init-ui)
;(require 'init-behavior)
;(require 'init-autocomplete)
;(require 'init-programming)
;(require 'init-helm)


(defvar backup-dir "~/.emacs.d/backups/")
(setq backup-directory-alist (list (cons "." backup-dir)))
(setq make-backup-files nil)
