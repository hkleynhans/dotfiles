;;; init-evil.el -- Initializes evil mode
;;; commentary:
;;;   Initialises evil mode.  It is worth looking at the following site:
;;;     https://github.com/bling/dotemacs/blob/master/config/init-evil.el
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
  (setq evil-search-module 'evil-search
	evil-magic 'very-magic
	evil-want-C-u-scroll t
	evil-want-C-w-in-emacs-state t
	evil-emacs-state-cursor '("red" box)
	evil-motion-state-cursor '("orange" box)
	evil-normal-state-cursor '("green" box)
	evil-visual-state-cursor '("orange" box)
	evil-replace-state-cursor '("red" bar)
	evil-operator-state-cursor '("red" hollow))
  :config
  (add-hook 'evil-mode-hook 'htk--config-evil)
  (evil-mode 1)
  (use-package evil-escape
    :ensure t
    :config
    (evil-escape-mode))

  (evil-put-property 'evil-state-properties 'normal    :tag " NORMAL ")
  (evil-put-property 'evil-state-properties 'insert    :tag " INSERT ")
  (evil-put-property 'evil-state-properties 'visual    :tag " VISUAL ")
  (evil-put-property 'evil-state-properties 'motion    :tag " MOTION ")
  (evil-put-property 'evil-state-properties 'emacs     :tag " EMACS ")
  (evil-put-property 'evil-state-properties 'replace   :tag " REPLACE ")
  (evil-put-property 'evil-state-properties 'operator  :tag " OPERATOR ")

  (use-package evil-leader
    :ensure t
    :config
    (evil-leader/set-leader "<SPC>")
    (evil-leader/set-key "f" 'helm-find-files)
    (evil-leader/set-key "b" 'helm-buffers-list)
    (evil-leader/set-key "s" 'helm-swoop)
    (evil-leader/set-key "x" 'helm-M-x)
    (evil-leader/set-key "cf" 'clang-format-buffer)
    (evil-leader/set-key "cc" 'clang-format-buffer)
    (global-evil-leader-mode))

  (use-package evil-surround
    :ensure t
    :config
    (global-evil-surround-mode))

  (use-package magit
    :ensure t
    :config
    (use-package evil-magit
      :ensure t)))

(provide 'init-evil)
;;; init-evil.el ends here
