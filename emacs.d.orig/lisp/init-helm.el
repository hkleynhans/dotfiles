;;; init-helm.el -- Initialize Helm
;;; Commentary:
;;; Code:
(use-package helm-config
  :ensure t
  :bind (("M-x" . helm-M-x)
	 ("C-x C-f" . helm-find-files)
	 ("C-x b" . helm-buffers-list))
  :config
  (use-package helm
    :ensure t
    :config
    (setq helm-buffers-fuzzy-matching t)
    (helm-mode 1))

  (use-package helm-gtags :ensure t)
  (use-package helm-package :ensure t)
  (use-package helm-flycheck :ensure t)
  (use-package helm-flyspell :ensure t)

  (use-package helm-ag
    :ensure t
    :config
    (setq helm-ag-base-command "/usr/local/bin/ag --nocolor --nogroup"))
  
  (use-package helm-swoop
    :ensure t
    :config
    (evil-leader/set-key "s" 'helm-swoop)
    ))
  
(use-package helm-projectile
  :ensure t
  :config
  (helm-projectile-on))


(provide 'init-helm)
;;; init-helm.el ends here
