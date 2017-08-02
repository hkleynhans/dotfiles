;; Emacs configuration for programming                  -*- mode: lisp -*-

; gtags
(setq gtags-suggested-key-mapping 1)
(require 'gtags)
(autoload 'gtags-mode "gtags" "" t)

(add-hook 'gtags-select-mode-hook
  '(lambda ()
     (setq hl-line-face 'underline)
     (hl-line-mode 1)
))


(defun gtags-update-single(filename)  
  "Update Gtags database for changes in a single file"
  (interactive)
  (start-process "update-gtags" "update-gtags" "bash" "-c" (concat "cd " (gtags-root-dir) " ; gtags --single-update " filename )))

(defun gtags-update-current-file()
  (interactive)
  (defvar filename)
  (setq filename (replace-regexp-in-string (gtags-root-dir) "." (buffer-file-name (current-buffer))))
  (gtags-update-single filename)
  (message "Gtags updated for %s" filename))

(defun gtags-update-hook()
  "Update GTAGS file incrementally upon saving a file"
  (when gtags-mode
    (when (gtags-root-dir)
      (gtags-update-current-file))))


(add-hook 'after-save-hook 'gtags-update-hook)



; Auto-complete
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
(ac-config-default)

; magit
(add-to-list 'load-path "~/.emacs.d/site-lisp/magit")
(require 'magit)

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



;; ----------------------------------- cedet ----------------------------------

; Enable project management features
;; (global-ede-mode 1)
;; (require 'semantic/sb)
;; (semantic-mode 1)
;; (which-function-mode 1)

;; ;; if you want to enable support for gnu global
;; (when (cedet-gnu-global-version-check t)
;;   (semanticdb-enable-gnu-global-databases 'c-mode)
;;   (semanticdb-enable-gnu-global-databases 'c++-mode))


;; (semantic-add-system-include "/usr/include" 'c++-mode)
;; (semantic-add-system-include "/bbsrc/source/proot/include/stlport/" 'c++-mode)
;; (semantic-add-system-include "/opt/swt/install/gcc-4.5.2/include/c++/4.5.2/" 'c++-mode)
;; (semantic-add-system-include "/bbsrc/source/proot/include/00depbuild/" 'c++-mode)
;; (semantic-add-system-include "/bbsrc/source/proot/include/00deployed/" 'c++-mode)
;; (semantic-add-system-include "/bbsrc/source/proot/include/00offlonly/" 'c++-mode)


;;(require 'python-mode)
(defun bbpy ()
  (interactive)
  (modify-syntax-entry ?_ "w")
  (c-set-offset 'arglist-close 0)
  (subword-mode t)
  (which-function-mode 1))

(add-hook 'python-mode-hook 'bbpy)
(setq python-shell-interpreter "/bb/bin/bbpy2.7")
