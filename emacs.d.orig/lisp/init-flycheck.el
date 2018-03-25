;;; init-flycheck.el -- Initializes flycheck mode
;;; commentary:
;;; code:
(use-package let-alist
  :ensure t)

(use-package flycheck
  :ensure t
  :config
  (use-package flycheck-pyflakes :ensure t)
  (add-hook 'after-init-hook 'global-flycheck-mode))

(provide 'init-flycheck)
;;; init-flycheck.el ends here
