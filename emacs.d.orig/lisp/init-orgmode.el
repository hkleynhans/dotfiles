;;; init-orgmode.el -- Initializes org mode
;;; Commentary:
;;; Code:
(use-package org-mode
  :ensure t
  :config
  (add-hook 'org-mode-hoook 'evil-org-mode))

(provide 'init-orgmode)
;;; init-orgmode.el ends here
