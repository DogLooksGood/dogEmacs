;;; -*- lexical-binding: t; -*-

(straight-use-package '(ligature :type git :host github :repo "mickeynp/ligature.el"))

(+pdump-packages 'ligature)

(require 'ligature)

(global-ligature-mode t)

(with-eval-after-load "ligature"
  (ligature-set-ligatures 'emacs-lisp-mode
                          '("->" "->>" "<=" ">="))
  (ligature-set-ligatures 'elixir-mode
                          '("->" "=>" "|>" "<-" ">=" "<=" "!=" "!==" "===" "==" "::" "++" "&&" "||" "<<" ">>"))
  (ligature-set-ligatures 'clojure-mode
                          '("->" "->>" ">=" "<="  ".-"))
  (ligature-set-ligatures 'web-mode
                          '("</" "<!--" "-->" "/>")))

(provide 'init-font)
