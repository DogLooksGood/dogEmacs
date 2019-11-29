;;; fira-code.el --- Summary -*- lexical-binding: t -*-
;;; Commentary:
;;;
;;; Emacs mode for displaying Fira Code ligatures using modified
;;; version of Fira Code called Fira Emacs.
;;;
;;; Code:

(require 'ligature-font)

(eval-when-compile
  (require 'cl))
(require 'dash)

(load "fira-code-data")

(defconst fira-code--word-ligatures
  (-keep
   (-lambda ([glyph input-string])
     (and (string-match-p "\\.liga$" glyph)
          (string-match-p "^[[:alpha:]]+$" input-string)
          input-string))
   fira-code--data)
  "List of ligatures that should be recognized when the occur
within a word.")

(defvar-local fira-code--disable-funcs nil)

(define-minor-mode fira-code-mode
  "Fira Code ligatures minor mode"
  :global nil
  (if fira-code-mode
      (progn
        (push (ligature-font--set-with-restore
               ligature-font-char-list
               fira-code--data)
              fira-code--disable-funcs)
        (push (ligature-font--set-with-restore
               ligature-font-word-ligatures
               fira-code--word-ligatures)
              fira-code--disable-funcs)
        (unless ligature-font-mode
          (push (lambda () (ligature-font-mode 0))
                fira-code--disable-funcs))
        (ligature-font-mode))
    (while fira-code--disable-funcs
      (funcall (pop fira-code--disable-funcs)))))

(defun fira-code-enable (&optional face)
  (fira-code-mode))

(provide 'fira-code)
