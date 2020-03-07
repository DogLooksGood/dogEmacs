(require 'package)

(setq package-archives
      '(("gnu"   . "http://elpa.emacs-china.org/gnu/")
        ("melpa" . "http://elpa.emacs-china.org/melpa/")
        ("melpa-stable" . "http://elpa.emacs-china.org/melpa-stable/")))

(setq package-selected-packages
      '(use-package quelpa-use-package company))

(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

(dolist (package package-selected-packages)
  (when (and (assq package package-archive-contents)
             (not (package-installed-p package)))
    (package-install package t)))

(setq quelpa-checkout-melpa-p nil)
(setq quelpa-update-melpa-p nil)
(setq use-package-always-ensure t)
(setq use-package-always-demand t)

(require 'use-package)
(require 'quelpa-use-package)
(quelpa-use-package-activate-advice)


;; (eval-buffer)
