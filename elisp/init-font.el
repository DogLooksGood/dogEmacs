;;; -*- lexical-binding: t; -*-

(straight-use-package '(ligature :type git :host github :repo "mickeynp/ligature.el"))

(+pdump-packages 'ligature)

(defvar +font-ligature t
  "If ligature is supported by current font.")

(when +font-ligature

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
                            '("</" "<!--" "-->" "/>"))))

;;; Fonts
;; Custom fonts can be set in ~/.emacs.d/private.el

(defvar +font-family "Fira Code")
(defvar +ufont-family "LXGW WenKai")
(defvar +fixed-pitch-family "Sarasa Mono SC")
(defvar +variable-pitch-family "Sarasa Gothic SC")
(defvar +font-size 11)

(defun +load-base-font ()
  (let* ((font-spec (format "%s-%d" +font-family +font-size))
         (variable-pitch-font-spec (format "%s-%d" +variable-pitch-family +font-size))
         (fixed-pitch-font-spec (format "%s-%d" +fixed-pitch-family +font-size)))
    (add-to-list 'default-frame-alist `(font . ,font-spec))
    (set-face-attribute 'variable-pitch nil :font variable-pitch-font-spec)
    (set-face-attribute 'fixed-pitch nil :font fixed-pitch-font-spec)))

(defun +load-ext-font ()
  (when window-system
    (dolist (charset '(kana han cjk-misc bopomofo))
      (set-fontset-font
       (frame-parameter nil 'font)
       charset
       (font-spec :family +ufont-family)))))

(defun +load-font ()
  (let* ((font-spec (format "%s-%d" +font-family +font-size))
         (variable-pitch-font-spec (format "%s-%d" +variable-pitch-family +font-size))
         (fixed-pitch-font-spec (format "%s-%d" +fixed-pitch-family +font-size)))
    (set-frame-font font-spec)
    (set-face-attribute 'variable-pitch nil :font variable-pitch-font-spec)
    (set-face-attribute 'fixed-pitch nil :font fixed-pitch-font-spec))
  (+load-ext-font))

;; Setup basic fonts
(+load-base-font)

;; `+load-ext-font' must run after frame created.
;; So we use `after-init-hook' here.
(add-hook 'after-init-hook '+load-ext-font)

(provide 'init-font)
