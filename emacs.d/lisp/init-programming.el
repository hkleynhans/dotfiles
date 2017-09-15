;;; init-programming.el -- Initializes programming modes
;;; Commentary:
;;; Code:
(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
	 ("\\.md\\'" . markdown-mode)
	 ("\\.markdown\\'" . markdown-mode))
  :config (setq markdown-command "/usr/local/bin/markdown"))

(defun htk--clangformat-hook ()
  "Call clang-format when saving."
  (add-hook 'before-save-hook 'clang-format-buffer))

(use-package clang-format
  :ensure t
  :config
  (add-hook 'c-mode-hook 'htk--clangformat-hook)
  (add-hook 'c++mode-hook 'htk--clangformat-hook))

(use-package magit
  :ensure t)

(use-package company
  :ensure t
  :config
  (use-package company-anaconda :ensure t)
  (use-package company-go :ensure t)
  (global-company-mode)
  (setq company-idle-delay 0.1)
  (setq company-minimum-prefix-length 1)
  (setq company-backends '((company-capf
			    company-dabbrev
			    company-files
			    company-gtags
			    company-elisp
			    company-go)))
  )

(use-package anaconda-mode :ensure t)

(defun htk--gohook ()
  "Set automatic go formatting in place."
  (add-hook 'before-save-hook 'gofmt-before-save))
  

(use-package go-mode
  :ensure t
  :config
  (add-to-list 'exec-path "~/go/bin")
  (add-hook 'go-mode-hook 'htk--gohook)
  (setq gofmt-command "goimports")

  (use-package golint :ensure t)

  (use-package go-eldoc
    :ensure t
    :config
    (go-eldoc-setup))
  (use-package go-autocomplete
    :ensure t))

(provide 'init-programming)
;;; init-programming.el ends here
