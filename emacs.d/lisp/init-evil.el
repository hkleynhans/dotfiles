;;; init-evil.el -- Initializes evil mode
;;; commentary:
;;; code:
(defun htk--config-evil ()
  "Configure evil mode."

  ;; Use Emacs state in these additonal modes.
  (dolist (mode '(ag-mode
		  term-mode))
    (add-to-list 'evil-emacs-state-modes mode))

  ;; Use insert state in these additional modes.
  (dolist (mode '(magit-log-edit-mode))
    (add-to-list 'evil-insert-state-modes mode)))

(use-package evil
  :ensure t
  :init
  (setq evil-want-C-u-scroll t)
  :config
  (add-hook 'evil-mode-hook 'htk--config-evil)
  (evil-mode 1)

  (use-package evil-leader
    :ensure t
    :config
    (evil-leader/set-leader "<SPC>")
    (global-evil-leader-mode))

  (use-package evil-surround
    :ensure t
    :config
    (global-evil-surround-mode)))


(provide 'init-evil)
;;; init-evil.el ends here
