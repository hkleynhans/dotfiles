;;; init-programming.el -- Initializes programming modes
;;; Commentary:
;;; Code:


(use-package auto-complete
  :ensure t
  :config
  (setq ac-use-quick-help t)
  (setq ac-ignore-case 'smart)
  (ac-config-default) ;; we use auto-complete
  )

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
