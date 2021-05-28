;;; init.el -- Emacs configuration file
;;; Commentary:
;;; Code:

;; Remove UI components
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
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

(use-package rainbow-delimiters)


;; --------------------------------------------------------------------------------
;; Editor changes
;; --------------------------------------------------------------------------------

;; Store backups and auto-save files in a single directory so that they don’t
;; clutter up my filesystem (or fail to be written on curlftpfs):
(let ((backupdir (format "%s/emacs-backups%d/" (or (getenv "XDG_RUNTIME_DIR") "/tmp") (user-uid))))
  (mkdir backupdir t)
  (setq backup-directory-alist `(("." . ,backupdir)))
  (setq auto-save-file-name-transforms
	`((".*" ,backupdir t))))


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

;; Display line and column numbers is all modes.
(setq line-number-mode t)
(setq column-number-mode t)


(use-package diminish)

(use-package ivy
  :init
  (ivy-mode 1))

(use-package counsel
  :after (ivy)
  :bind (("C-x C-f" . counsel-find-file)
	 ("<f5>" . compile))
  )

(use-package swiper
  :bind (("C-s" . swiper-isearch)
	 ("C-r" . swiper-isearch-backwards)
	 ("C-q" . swiper-isearch-thing-at-point))
  )

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

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :config (progn
	    (setq lsp-auto-guess-root t)
	    (setq lsp-prefer-flymake nil))
  :hook ((go-mode . lsp-deferred)))

(use-package lsp-ui
  :commands lsp-ui-mode
  :config
  (setq lsp-ui-sideline-enable nil
	lsp-ui-doc-enable nil
	lsp-ui-flymake-enable nil
	lsp-ui-imenu-enable nil
	lsp-ui-sideline-ignore-duplicate t))

(use-package flycheck
  :init (add-hook 'after-init-hook 'global-flycheck-mode))

(use-package company
  :init
  (add-hook 'after-init-hook 'global-company-mode))

(use-package company-lsp
  :commands company-lsp
  :config (push 'company-lsp company-backends))

(use-package magit)

(use-package editorconfig
  :config
  (editorconfig-mode 1))

(use-package ace-window
  :bind ("M-o" . ace-window))

(use-package which-key
  :config
  (which-key-mode 1))

;; join line to next line
(global-set-key (kbd "C-j")
            (lambda ()
                  (interactive)
                  (join-line -1)))

;;; init.el ends here
