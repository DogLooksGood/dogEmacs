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
(require 'the-git)
(require 'the-ivy)
(require 'the-prog)
(require 'the-lisp)
(require 'the-clojure)
(require 'the-elixir)
(require 'the-rust)
(require 'the-sql)
(require 'the-completion)
(require 'the-window)
(require 'the-edit)
(require 'the-project)
(require 'the-visual)
(require 'the-eshell)
(require 'the-dired)
(require 'the-conf)
(require 'the-server)
(require 'the-org)
(require 'the-treemacs)
(require 'the-modal)
