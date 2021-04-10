;;; -*- lexical-binding: t -*-

(require 'joker-theme)
(require 'storybook-theme)

;;; No scroll bar
(scroll-bar-mode -1)

;;; No tool bar
(tool-bar-mode -1)

;;; No menu bar
(menu-bar-mode -1)

;;; Use window divider
(window-divider-mode 1)

;;; No cursor blink
(add-hook 'after-init-hook (lambda () (blink-cursor-mode -1)))

;;; Nice window divider

(set-display-table-slot standard-display-table
                        'vertical-border
                        (make-glyph-code ?┃))

;;; No fringe in minibuffer

(add-hook 'after-make-frame-functions
          (lambda (frame)
            (set-window-fringes
             (minibuffer-window frame) 0 0 nil t)))

;;; Margin

(let ((margin 0))
  (add-to-list 'default-frame-alist (cons 'internal-border-width margin)))

;;; Transparency

(let ((alpha 100))
  (add-to-list 'default-frame-alist (cons 'alpha alpha)))

;;; No window decoration

(add-to-list 'default-frame-alist (cons 'undecorated t))

;;; Fonts

(defvar +font-family "Fira Code")
(defvar +ufont-family "WenQuanYi Micro Hei")
(defvar +fixed-pitch-family "Sarasa Mono SC")
(defvar +variable-pitch-family "Sarasa Gothic SC")
(defvar +font-size 11)

;;; (+load-font)

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

(+load-base-font)

(add-hook 'after-init-hook '+load-ext-font)

;;; Theme

(defvar +after-change-theme-hook nil
  "Hooks called after theme is changed.")

(defvar +theme-list '(joker printed storybook))

(defun +change-theme (&optional init)
  (interactive)
  (let ((enabled-themes custom-enabled-themes)
	(theme (car +theme-list)))
    (load-theme theme t)
    (setq +theme-list (append (cdr +theme-list) (list (car +theme-list))))
    (unless init
      (+load-font)
      (message "Load theme: %s" theme)
      (run-hook-with-args '+after-change-theme-hook theme))
    (mapc #'disable-theme enabled-themes)))

(+change-theme t)

(provide 'init-laf)
