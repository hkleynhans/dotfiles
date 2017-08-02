(c-add-style "my-c-style"
             '("stroustrup"
               (c-basic-offset . 2)
               ))

(defun my-c-mode-hook ()
;  (gtags-mode 1)
  (c-set-style "my-c-style")
  (c-set-offset 'arglist-intro '++)
  (c-set-offset 'substatement-open 0) ; brackets should be at same indentation level as the statements they open
  (c-set-offset 'innamespace 0)
  (c-set-offset 'inline-open '+)
  (c-set-offset 'block-open '+)
  (c-set-offset 'brace-list-open '+)   ; all "opens" should be indented by the c-indent-level
  (c-set-offset 'case-label '+)        ; indent case labels by c-indent-level, too
  (setq tab-width 2)
  (setq indent-tabs-mode nil)
  (setq require-final-newline t))

(add-hook 'c-mode-hook 'my-c-mode-hook)
(add-hook 'c++-mode-hook 'my-c-mode-hook)

(add-to-list 'auto-mode-alist '("\\.h$" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.inc$" . fortran-mode))

(add-to-list 'auto-mode-alist '("\\.xml$" . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.gobxml$" . nxml-mode))

(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)

(add-to-list 'auto-mode-alist '("\\.ml[ily]?$" . tuareg-mode))
(add-to-list 'auto-mode-alist '("\\.topml$" . tuareg-mode))


;; (require 'ggtags)
;; (add-hook 'c-mode-common-hook
;;           (lambda ()
;;             (when (derived-mode-p 'c-mode 'c++-mode 'java-mode 'asm-mode)
;;               (ggtags-mode 1))))

(provide 'init-programming)
