;;; init-elpa.el -- Iinitializes the package manager
;;; commentary:
;;; code:

;;; Standard package repositories
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("elpy" . "https://jorgenschaefer.github.io/packages/"))

(require 'epa-file)
(custom-set-variables '(epg-gpg-program "/usr/local/bin/gpg2"))
(epa-file-enable)

;;; Pin some packages to specific repositories.
(setq package-pinned-packages '((gtags . "marmalade")))

;; Don't do this, because we already did it (in init.el)
(setq package-enable-at-startup nil)

(require 'cl-lib)
(provide 'init-elpa)
;;; init-elpa.el ends here
