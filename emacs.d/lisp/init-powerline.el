;;; init-powerline.el -- Initialize evil powerline
;;; Commentary:
;;; Code:
(when (memq window-system '(mac ns))
  (setq ns-use-srgb-colorspace nil))

(use-package powerline
  :ensure t)

;; bad coment over here
(use-package smart-mode-line-powerline-theme
  :ensure t)

(use-package smart-mode-line
  :ensure t
  :config
  (require 'powerline)
  (setq powerline-default-separator 'arrow-fade)
  (setq sml/theme 'powerline)
  (sml/setup)
  ;; These colors are more pleasing (for gruvbox)
  (custom-theme-set-faces
   'user
   '(powerline-evil-normal-face ((t (:inherit powerline-evil-base-face :background "chartreuse3"))))
   '(sml/folder ((t (:inherit sml/global :background "grey22" :foreground "grey50" :weight normal))) t)
   '(sml/git ((t (:background "grey22" :foreground "chartreuse3"))) t)))

(provide 'init-powerline)
;;; init-powerline.el ends here
