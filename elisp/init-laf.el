;;; -*- lexical-binding: t -*-

(require 'printed-theme)
(require 'minidark-theme)

;;; No scroll bar
(setq-default scroll-bar-width 10)
(scroll-bar-mode -1)

;;; No tool bar
(tool-bar-mode -1)

;;; No menu bar
(menu-bar-mode -1)

;;; Use window divider
(window-divider-mode 1)

;;; No blink cursor
(blink-cursor-mode -1)

;;; No window divider
(set-display-table-slot standard-display-table
                        'vertical-border
                        (make-glyph-code ? ))

;;; No fringe in minibuffer

(add-hook 'after-make-frame-functions
          (lambda (frame)
            (set-window-scroll-bars
             (minibuffer-window frame) 0 nil 0 nil t)
            (set-window-fringes
             (minibuffer-window frame) 0 0 nil t)))

;;; Margin

;; (let ((margin 0))
;;   (add-to-list 'default-frame-alist (cons 'internal-border-width margin)))

;;; Transparency

;; (let ((alpha 100))
;;   (add-to-list 'default-frame-alist (cons 'alpha alpha)))

;;; No window decoration

;; (add-to-list 'default-frame-alist (cons 'undecorated t))

;;; Theme

(defvar +after-change-theme-hook nil
  "Hooks called after theme is changed.")

(defvar +theme-list '(printed minidark ))

(defun +change-theme ()
  (interactive)
  (let ((enabled-themes custom-enabled-themes)
        (theme (car +theme-list)))
    (when theme
      (load-theme theme t))
    (mapc #'disable-theme (remove theme enabled-themes))
    (setq +theme-list (append (cdr +theme-list) (list (car +theme-list))))
    (message "Load theme: %s" theme)
    (run-hook-with-args '+after-change-theme-hook theme)
    (+load-font)))

;; Load the first theme in `+theme-list'.
(when-let ((theme (car +theme-list)))
  (load-theme theme t))

(provide 'init-laf)
