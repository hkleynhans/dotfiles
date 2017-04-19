;; Emacs configuration file                             -*- mode: lisp -*-

(let ((min-version "24.1"))
    (when (version< emacs-version min-version)
          (error "This config requires at least Emacs %s, but you're running %s"
                            min-version emacs-version)))

;; Use this file for HTTP proxy settings if needed, for packages.
(when (file-exists-p "~/.emacs.d/init-local-prolog.el")
  (load "~/.emacs.d/init-local-prolog.el"))


;; Package System
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)

(package-initialize)

;; Load the packages we want
(let ((required-packages '(auto-complete
                           auto-complete-c-headers
                           fzf
                           ggtags
                           company
                           rtags
                           helm
                           helm-gtags
                           ido-ubiquitous))
      (has-refreshed nil))
  (dolist (p required-packages)
    (unless (package-installed-p p)
      (unless has-refreshed
        (message "Refreshing package database...")
        (package-refresh-contents)
        (setq has-refreshed-t)
        (message "Done."))
      (package-install p))))



(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))


(require 'init-ui)
(require 'init-behavior)
(require 'init-autocomplete)
(require 'init-programming)
(require 'init-helm)
