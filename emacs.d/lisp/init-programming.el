;;; init-programming.el -- Initializes programming modes
;;; Commentary:
;;; Code:


(use-package company
  :ensure t
  :config
  (use-package company-anaconda :ensure t)
  (use-package company-go :ensure t)
  (global-company-mode)
  (setq company-idle-delay 0.1)
  (setq company-minimum-prefix-length 1)
  (setq company-backends '((company-capf
			    company-files
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

  (use-package go-eldoc
    :ensure t
    :config
    (go-eldoc-setup))
  (use-package go-autocomplete
    :ensure t))

(provide 'init-programming)
;;; init-programming.el ends here
