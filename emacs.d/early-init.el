;;; early-init.el -- Early emacs initialization
;;; Commentary:
;;; Code:

;; --------------------------------------------------------------------------------
;; Disable UI
;; --------------------------------------------------------------------------------
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(horizontal-scroll-bar-mode -1)

;; GarbageCollection
(setq gc-cons-threshold 402653184
      gc-cons-percentage 0.6)
;; -GarbageCollection

;; --------------------------------------------------------------------------------
;; Bootstrap straight package manager
;; --------------------------------------------------------------------------------

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
	(url-retrieve-synchronously
	 "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
	 'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)

(setq straight-use-package-by-default t)
(setq package-enable-at-startup nil)
(setq inhibit-startup-message t)


(provide 'early-init)
;;; early-init.el ends here
