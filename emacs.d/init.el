;;; init.el --- Emacs configuration
;;; Commentary:
;;; Code:

;; Disable interface elements
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

(require 'cl)

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

(require 'ansi-color)
(setq ansi-color-for-comint-mode t)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'bbg-style)
(require 'move-border)

(global-set-key (kbd "M-o") 'other-window)
(global-set-key (kbd "M-<down>") 'move-border-down)
(global-set-key (kbd "M-<up>") 'move-border-up)
(global-set-key (kbd "M-<right>") 'move-border-right)
(global-set-key (kbd "M-<left>") 'move-border-left)

;; Use UTF-8 Unix line endings by default.
(setq buffer-file-coding-system 'utf-8-unix)

(defvar htk-packages
  '(company clang-format company-go company-c-headers company-glsl
            counsel-etags counsel docker edit-server flycheck
            flycheck-clangcheck flyspell glsl-mode go-mode groovy-mode ivy
            magit python cmake-mode smooth-scrolling use-package x509-mode)
  "A list of packages to ensure are installed at launch.")

(defun htk-packages-installed-p ()
  (loop for p in htk-packages
        when (not (package-installed-p p)) do (return nil)
        finally (return t)))


(unless (htk-packages-installed-p)
  ;; check for new packages (package versions)
  (message "%s" "Refreshing the package database...")
  (package-refresh-contents)
  (message "%s" " done.")
  ;; install the missing packages
  (dolist (p htk-packages)
    (when (not (package-installed-p p))
      (package-install p))))



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

;; Show trailing whitespace as well as tabs.  Remove trailing whitespace on
;; save.
(setq-default show-trailing-whitespace t)
(setq-default show-tabs t)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Replace tabs with spaces.
(setq-default indent-tabs-mode nil)

(use-package cc-mode
  :mode (("\\.h\\(h?\\|xx\\|pp\\)\\'" . c++-mode)
         ("\\.m\\'" . c-mode)
         ("\\.mm\\'" . c++-mode))
  :hook (c-mode-common . (lambda()
			   (flyspell-prog-mode))))


(use-package clang-format
  :diminish
  :bind (("C-c u" . clang-format-buffer)
         ("C-c /" . clang-format-region)))

(use-package go-mode
  :config
  (add-hook 'before-save-hook #'gofmt-before-save))

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

(use-package company-c-headers
  :after company
  :config
  (push 'company-c-headers company-backends))

(use-package company-go
  :after company
  :config
  (push 'company-go company-backends))

(use-package glsl-mode :ensure t)
(use-package company-glsl
  :ensure t
  :config
  (push 'company-glsl company-backends))

(use-package docker
  :ensure t
  :config
  (docker-global-mode t))

(use-package dockerfile-mode
  :mode (("\\.Dockerfile\\'" . dockerfile-mode))
  :ensure t)



;; Emacs server
(use-package edit-server
  :if window-system
  :init
  (add-hook 'after-init-hook 'server-start t)
  (add-hook 'after-init-hook 'edit-server-start t))

(use-package ivy
  :demand t
  :diminish
  :bind (("C-c C-r" . ivy-resume))
  :config
  (ivy-mode 1)
  (ivy-set-occur 'ivy-switch-buffer 'ivy-switch-buffer-occur))

(use-package compile
  :no-require
  :bind (("C-c i" . compile)
	 ("C-c c" . recompile)))

(use-package counsel
  :after ivy
  :bind (("C-s" . swiper)
         ("C-c g" . counsel-git)
         ("C-c j" . counsel-git-grep)
         ("C-c k" . counsel-ag)
         ("C-x C-f" . counsel-find-file)
         ("C-h f" . counsel-describe-function)
         ("M-x" . counsel-M-x))
  :demand t
  :diminish
  :config
  (counsel-mode 1))

(use-package counsel-etags
  :after (counsel))

(use-package flycheck
  :diminish
  :config
  (global-flycheck-mode))

(use-package flycheck-clangcheck
  :diminish
  :after flycheck
  :config
  (defun my-select-clangcheck-for-checker ()
    "Select clang-check for flycheck's checker."
    (flycheck-select-checker 'c/c++-clangcheck))

  (add-hook 'c-mode-hook #'my-select-clangcheck-for-checker)
  (add-hook 'c++-mode-hook #'my-select-clangcheck-for-checker)
  (setq flycheck-clangcheck-analyze t))

(use-package flyspell
  :diminish
  :config
  (setq ispell-dictionary "british")
  (add-hook 'text-mode-hook 'flyspell-mode)
  (add-hook 'prog-mode-hook 'flyspell-prog-mode))

(use-package magit :ensure t)

(use-package smooth-scrolling
  :ensure t
  :config
  (smooth-scrolling-mode 1))

(use-package x509-mode :ensure t)
(use-package groovy-mode
  :ensure t
  :mode (("\\Jenkinsfile\\'" . groovy-mode)))

(provide 'init)

;;; init.el ends here
