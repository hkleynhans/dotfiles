;;; init-spel.el -- Setup flyspell
;;; Commentary:
;;; Code:
(use-package flyspell
  :ensure t
  :init
  (use-package ispell
    :ensure t
    :config
    (setq ispell-program-name "aspell"
	  ispell-list-command "--list"))
  :config
  (add-hook 'text-mode-hook 'flyspell-mode)
  (add-hook 'prog-mode-hook 'flyspell-prog-mode))

(provide 'init-spell)
;;; init-spell.el ends here
