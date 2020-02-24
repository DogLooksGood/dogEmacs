(require 'package)
;; load autoload files and populate load-path’s
(package-initialize)

(add-to-list 'load-path "~/.emacs.d/elisp/")
(setq user/dumped-load-path load-path)

(dolist (package '(paredit
                   emmet-mode
                   nov
                   dash
                   string-inflection
                   seq
                   company
                   company-posframe
                   smartparens
                   flycheck
                   lsp-mode
                   lsp-ui
                   dumb-jump
                   elixir-mode
                   rust-mode
                   markdown-mode
                   hideshow
                   highlight-numbers
                   yascroll
                   focus
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
                   w3m
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
                   the-global))
  (require package))

(load-theme 'joker t t)

;;; We have to unload tramp in dump, otherwise tramp will not work.
(tramp-unload-tramp)

;; dump image
(dump-emacs-portable "~/.emacs.d/emacs.pdmp")
