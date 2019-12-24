(require 'package)

(setq package-archives
      '(("gnu"   . "http://elpa.emacs-china.org/gnu/")
        ("melpa" . "http://elpa.emacs-china.org/melpa/")
        ("melpa-stable" . "http://elpa.emacs-china.org/melpa-stable/")))

(setq package-selected-packages
      '(use-package quelpa-use-package))

(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

(dolist (package package-selected-packages)
  (when (and (assq package package-archive-contents)
             (not (package-installed-p package)))
    (package-install package t)))

(setq quelpa-checkout-melpa-p nil)
(setq use-package-always-ensure t)
(setq quelpa-update-melpa-p nil)

;; Requires
(require 'use-package)
(require 'quelpa-use-package)
(quelpa-use-package-activate-advice)

(add-to-list 'load-path "~/.emacs.d/elisp/")
(add-to-list 'load-path "~/.emacs.d/elisp2/")

(require 'the-elisp)
(require 'the-look-and-feel)
(require 'the-global)
(require 'the-git)
(require 'the-ivy)
(require 'the-prog)
(require 'the-completion)
(require 'the-lisp)
(require 'the-clojure)
;; (require 'the-java)
;; (require 'the-haskell)
;; (require 'the-elixir)
(require 'the-rust)
;; (require 'the-golang)
(require 'the-sql)
(require 'the-html)
(require 'the-window)
(require 'the-edit)
(require 'the-nav)
(require 'the-project)
(require 'the-visual)
(require 'the-eshell)
(require 'the-dired)
(require 'the-conf)
(require 'the-org)
(require 'the-snippet)
(require 'the-modal)
(require 'the-latex)
;; (require 'the-ebook)
(require 'the-tab)
(require 'the-docker)
(require 'the-server)
