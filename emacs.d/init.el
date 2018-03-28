;; Disable interface elements
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))


;;; Standard package repositories
(require 'package)
 (setq package-enable-at-startup nil)
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("elpy" . "https://jorgenschaefer.github.io/packages/"))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(require 'diminish)
(require 'bind-key)

(require 'cl-lib)

(setq inhibit-startup-message t)

;; Keep emacs custom settings in a separate file
(setq custom-file(expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

;; Keep backups in the temp folder.
(setq backup-by-copying t      ; don't clobber symlinks
      backup-directory-alist
      '(("." . "~/.emacs-saves/"))   ; don't litter my fs tree
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)       ; use versioned backups

;;(defun init--package-install (package)
;;  (when (not (package-installed-p package))
;;    (package-install package))
;;  (delete-other-windows))

;; Install extensions if they are missing
;; (defun init--install-packages ()
;;(init--package-install "magit")
;;  (init--package-install "markdown-mode"))

;; (condition-case nil
;; (init--install-packages)
;;   (error
;;    (package-refresh-contents)
;;    (init--install-packages)))

(setq font-lock-maximum-decoration t
      color-theme-is-global t
      truncate-partial-width-windows nil)

;; highlight the current line
(setq global-hl-mode 1)

(show-paren-mode 1)
(global-font-lock-mode t)
(transient-mark-mode 1)
(defalias 'yes-or-no-p 'y-or-n-p)

;; always display line and column number
(setq line-number-mode t)
(setq column-number-mode t)

(when window-system
  (tooltip-mode -1)
  (blink-cursor-mode -1))

;; Don't beep. Don't visible bell.
(setq visible-bell nil)
(setq ring-bell-function (lambda ()
			   (invert-face 'mode-line)
			   (run-with-timer 0.05 nil 'invert-face 'mode-line)))

(load-theme 'gruvbox)
(split-window-horizontally)
(setq truncate-partial-width-windows nil)
(add-to-list 'default-frame-alist '(fullscreen . maximized))
;;(set-face-attribute 'default nil :font "Liberation Mono-10" )
;;(set-frame-font "Liberation Mono-10" nil t)

(use-package company
  :ensure t
  :diminish
  :init
  (add-hook 'after-init-hook 'global-company-mode)
  :config
  (setq company-idle-delay 0)
  (global-company-mode 1))

(use-package company-elisp
  :after company
  :config
  (push 'company-elisp company-backends))

(use-package company-go
  :after company
  :config
  (push 'company-go company-backends))

;; (use-package ido-vertical-mode
;;   ;; Experimenting with ivy-mode
;;   :disabled t
;;   :init
;;   (ido-mode 1)
;;   (ido-vertical-mode 1)
;;   (setq ido-vertical-define-keys 'C-n-and-C-p-only))

;; Emacs server
(use-package edit-server
  :if window-system
  :init
  (add-hook 'after-init-hook 'server-start t)
  (add-hook 'after-init-hook 'edit-server-start t))

(use-package ivy
  :diminish
  :config
  (ivy-mode 1)
  (ivy-set-occur 'ivy-switch-buffer 'ivy-switch-buffer-occur))

(use-package compile
  :no-require
  :bind (("C-c c" . compile)))

(use-package counsel
  :after ivy
  :demand t
  :diminish
  :config
  (counsel-mode 1))

(use-package counsel-projectile
  :after (counsel projectile)
  :config
  (counsel-projectile-mode))

(use-package flycheck)

(use-package flyspell)

(use-package magit :ensure t)

(use-package projectile
  :ensure t
  :diminish
  :config
  (projectile-global-mode))

(use-package smooth-scrolling
  :ensure t
  :config
  (smooth-scrolling-mode 1))
