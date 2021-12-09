;;; -*- lexical-binding: t -*-

(straight-use-package 'meow)

(defun meow-setup ()
  ;; Programmer Dvorak layout on ansi keyboard
  (setq meow-cheatsheet-physical-layout meow-cheatsheet-physical-layout-ansi
        meow-cheatsheet-layout meow-cheatsheet-layout-dvp)
  ;; it's not a good idea to have a complex leader keymap
  ;; here we create bindings for necessary, high frequency commands
  (meow-leader-define-key
