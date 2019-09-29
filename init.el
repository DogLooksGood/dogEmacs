(require 'package)

(setq package-archives
      '(("gnu"   . "http://elpa.emacs-china.org/gnu/")
        ("melpa" . "http://elpa.emacs-china.org/melpa/")))

(setq package-selected-packages
      '(use-package))

(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

(dolist (package package-selected-packages)
  (when (and (assq package package-archive-contents)
             (not (package-installed-p package)))
    (package-install package t)))

;; Requires
(require 'use-package)

(setq use-package-always-ensure t)

(add-to-list 'load-path "~/.emacs.d/elisp/")

(require 'the-look-and-feel)
(require 'the-global)
(require 'the-modal)
(require 'the-w3m)
(require 'the-magit)
(require 'the-ivy)
(require 'the-lisp)
(require 'the-clojure)
(require 'the-elixir)
(require 'the-company)
(require 'the-window)
(require 'the-multi-edit)
(require 'the-expand-region)
(require 'the-projectile)
(require 'the-eshell)
(require 'the-dired)
(require 'the-sql)
(require 'the-parens)
(require 'the-rust)
(require 'the-lsp)
(require 'the-conf)
(require 'the-display)
(require 'the-keys)
(require 'the-server)
(require 'the-org)
