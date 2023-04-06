;;; init.el -- Emacs configuration file
;;; Commentary:
;;; Code:


(setq user-mail-address "henry.kleynhans@gmail.com")
(setq user-full-name "Henry Kleynhans")

(global-hl-line-mode 1)

;; --------------------------------------------------------------------------------
;; Themes
;; --------------------------------------------------------------------------------

;; Configure a default custom fase.
(setq default-frame-alist '((font . "Fira Code 12")))

;; Use the doom-dracula color scheme
(use-package doom-themes
  :init
  (load-theme 'doom-dracula t))

(use-package solaire-mode
  :init
  (solaire-global-mode))

(use-package rainbow-delimiters
  :init
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

;; --------------------------------------------------------------------------------
;; Editor changes
;; --------------------------------------------------------------------------------

;; Common keyboard shortcuts
(global-set-key (kbd "C-w") 'backward-kill-word)
(global-set-key (kbd "C-x C-k") 'kill-region)
(global-set-key (kbd "C-c C-k") 'kill-region)
(global-set-key (kbd "C-x C-m") 'execute-extended-command)
(global-set-key (kbd "C-x C-m") 'execute-extended-command)
(global-set-key (kbd "M-p") 'fill-paragraph)

;; Store backups and auto-save files in a single directory so that they don’t
;; clutter up my filesystem (or fail to be written on curlftpfs):
(defvar hk-backup-directory (concat user-emacs-directory "backups"))
(if (not (file-exists-p hk-backup-directory))
    (make-directory hk-backup-directory))
(setq backup-directory-alist `(("." . ,hk-backup-directory)))
(setq make-backup-files t
      backup-by-copying t
      version-control t
      delete-old-versions t
      kept-old-versions 6
      kept-new-versions 9
      )

(let ((my-auto-save-dir (locate-user-emacs-file "auto-save")))
  (setq auto-save-file-name-transforms
        `((".*" ,(expand-file-name "\\2" my-auto-save-dir) t)))
  (unless (file-exists-p my-auto-save-dir)
    (make-directory my-auto-save-dir)))
(setq auto-save-default t
      auto-save-timeout 10
      auto-save-interval 200)

;; Use the command key as the emacs meta key.
(setq mac-command-modifier 'meta)

;; Stop playing noises.
(setq ring-bell-function 'ignore)
(setq visible-bell t)

;; Don't ask to save files before compilation, just save them.
(setq compilation-ask-about-save nil)

;; Don't ask to kill the current compilation, just kill it.
(setq compilation-always-kill t)

;; All prompts should be y or n
(defalias 'yes-or-no-p 'y-or-n-p)

;; Automatically revert buffers when files change.
(global-auto-revert-mode t)

;; Default fill column of 80 characters for line wrapping using M-q
(setq-default fill-column 80)

;; Show matching parenthesis
(show-paren-mode t)
(setq show-paren-style 'mixed)

;; Display line and column numbers is all modes.
(setq line-number-mode t)
(setq column-number-mode t)

;; Show column numbers for some modes
(dolist (mode '(text-mode-hook
		prog-mode-hook
		conf-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 1))))

;; Use dooom modeline.  This requires manually running:
;;    M-x all-the-icons-install-fonts
;; one time.
(use-package all-the-icons :ensure t)

(use-package doom-modeline
  :init (doom-modeline-mode t)
  :custom (doom-modeline-height 15))

;; Override some modes which derive from the above.
(dolist (mode '(org-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; Do not warn on large files
(setq large-file-warning-threshold nil)

;; do not warn when following symlinks
(setq vc-follow-symlinks t)

;; do not warn when advice is added for a function
(setq ad-redefinition-action 'accept)

(use-package diminish)

(use-package eldoc
  :diminish eldoc-mode)

(use-package ivy
  :diminish ivy-mode
  :init
  (ivy-mode 1))

(use-package expand-region
  :bind (("C-=" . er/expand-region)
	 ("C--" . er/contract-region)))

(use-package counsel
  :after ivy
  :bind (("C-x C-f" . counsel-find-file)
	 ("C-<f5>" . compile)
  	 ("<f5>" . recompile))
  )

(use-package swiper
  :bind (("C-s" . swiper-isearch)
	 ("C-r" . swiper-isearch-backwards)
	 ("C-q" . swiper-isearch-thing-at-point))
  )

(use-package clang-format
  :ensure
  :bind(("C-c TAB" . clang-format-region))
  :config
  (setq clang-format-style "file")
  (setq clang-format-fallback-style "llvm"))

(use-package go-mode
  :init (add-hook 'go-mode-hook
		  (lambda()
		    (add-hook 'before-save-hook 'gofmt-before-save)
		    (if (not (string-match "go" compile-command))
			(set (make-local-variable 'compile-command)
			     "go build -v && go test -v && go vet"))
		    (setq gofmt-command "goimports")
		    (setq truncate-lines t)
		    (setq indent-tabs-mode t)
		    (setq tab-width 8))))

(use-package rustic
  :ensure
  :config
  (setq rustic-format-on-save t))

(use-package lsp-mode
  :init (setq lsp-keymap-prefix "C-c l")
  :commands (lsp lsp-deferred)
  :config (progn
	    (setq lsp-auto-guess-root t))
  :hook ((go-mode . lsp-deferred)
	 (c-mode . lsp-deferred)
	 (c++-mode . lsp-deferred)
	 (python-mode . lsp-deferred)
	 (rust-mode . lsp-deferred)))

(use-package lsp-pyright)

(use-package lsp-ui
  :commands lsp-ui-mode
  :config
  (setq lsp-ui-sideline-enable nil
	lsp-ui-doc-enable nil
	lsp-ui-imenu-enable nil
	lsp-ui-sideline-ignore-duplicate t))

(use-package lsp-ivy
  :commands lsp-ivy-workspace-symbol)

(use-package dap-mode
  :config
  (require 'dap-go)
  (require 'dap-python)
  (require 'dap-lldb))

(use-package flycheck
  :diminish flycheck-mode
  :init (add-hook 'after-init-hook 'global-flycheck-mode))

(use-package company
  :diminish company-mode
  :config
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 1)
  :init
  (add-hook 'after-init-hook 'global-company-mode))

(use-package company-box
  :diminish company-box-mode
  :hook (company-mode . company-box-mode))

(use-package company-lsp
  :commands company-lsp
  :config (push 'company-lsp company-backends))

(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :custom ((projectile-completion-system 'ivy))
  :bind-keymap ("C-c p" . projectile-command-map)
  :init
  (when (file-directory-p "~/Projects")
    (setq projectile-project-search-path '("~/Projects")))
  (setq projectile-search-project-action #'projectile-dired))

(use-package counsel-projectile
  :diminish counsel-projectile-mode
  :config (counsel-projectile-mode))

(use-package go-projectile
  :diminish)

(use-package magit)

(use-package editorconfig
  :diminish editorconfig-mode
  :config
  (editorconfig-mode 1))

(use-package ace-window
  :bind ("M-o" . ace-window))

(use-package ace-jump-mode
  :bind ("C-;" . ace-jump-mode))

(use-package which-key
  :diminish which-key-mode
  :config
  (which-key-mode 1))

(use-package org
  :config
  (setq org-directory "~/org")
  (setq org-log-done t)
  (setq org-startup-indented t))
  
;; join line to next line
(global-set-key (kbd "C-j")
            (lambda ()
                  (interactive)
                  (join-line -1)))

(toggle-frame-fullscreen)

;;; init.el ends here
