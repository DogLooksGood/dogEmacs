(require 'package)
;; load autoload files and populate load-path’s
(package-initialize)

(add-to-list 'load-path "~/.emacs.d/elisp/")
(setq user/dumped-load-path load-path)

(setq package-archives
      '(("gnu"   . "http://elpa.emacs-china.org/gnu/")
        ("melpa" . "http://elpa.emacs-china.org/melpa/")
        ("melpa-stable" . "http://elpa.emacs-china.org/melpa-stable/")))
(setq quelpa-checkout-melpa-p nil)
(setq quelpa-update-melpa-p nil)
(setq use-package-always-ensure t)
(setq use-package-always-demand t)

(dolist (package '(paredit
                   emmet-mode
                   nov
                   dash
                   string-inflection
                   seq
                   company
                   smartparens
                   flycheck
                   lsp-mode
                   lsp-ui
                   dumb-jump
                   rust-mode
                   markdown-mode
                   hideshow
                   highlight-numbers
                   yascroll
                   xml
                   css-mode
                   highlight-symbol
                   swiper
                   dired-hide-dotfiles
                   dired-sidebar
                   yasnippet
                   clojure-mode
                   clj-refactor
                   cider
                   use-package
                   quelpa-use-package
                   dockerfile-mode
                   docker
                   ivy
                   counsel
                   counsel-projectile
                   meghanada
                   haskell-mode
                   intero
                   deft
                   web-mode
                   vue-mode
                   css-mode
                   json
                   mini-modeline
                   magit
                   diff-hl
                   gitignore-mode
                   go-mode
                   shiftless
                   eshell
                   ace-window
                   org
                   org-bullets
                   flyspell
                   yaml-mode
                   toml-mode
                   multiple-cursors
                   wgrep
                   hydra
                   ripgrep
                   projectile
                   m4d
                   joker-theme
                   shiftless
                   rime
                   ;; Try to dump a few config files
                   the-global
                   the-look-and-feel
                   the-ivy
                   the-snippet
                   the-completion
                   the-org
                   the-lisp
                   the-clojure
                   the-project
                   the-edit
                   the-visual))
  (require package))

(load-theme 'joker t t)

;;; We have to unload tramp in dump, otherwise tramp will not work.
(tramp-unload-tramp)

;; dump image
(dump-emacs-portable "~/.emacs.d/emacs.pdmp")
