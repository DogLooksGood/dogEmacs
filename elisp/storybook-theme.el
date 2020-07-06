;;; -*- lexical-binding: t -*-

(deftheme storybook "A simple medium contrast light theme.")

(let ((height-comment 85)
      (height-code 80)
      (height-header 125))
  (custom-theme-set-faces
   'storybook
   `(default                        ((t (:height ,height-code :background "#f2f2f2" :foreground "#222222"))))
   `(cursor                         ((t (:background "black"))))
   '(region                         ((t (:background "#AFCFFF"))))
   `(hl-line                        ((t (:underline "#999" :overline "#999" :background "#e9e9e9"))))
   `(font-lock-comment-face         ((t (:slant italic :height ,height-comment :foreground "VioletRed4" :extend t))))
   `(font-lock-doc-face             ((t (:slant italic :height ,height-comment :foreground "VioletRed4" :extend t))))
   `(font-lock-warning-face         ((t (:foreground "red"))))
   `(font-lock-string-face          ((t (:foreground "#909090"))))
   `(font-lock-function-name-face   ((t (:height ,height-header))))
   `(font-lock-keyword-face         ((t ())))
   `(font-lock-constant-face        ((t (:foreground "#0077AA"))))
   `(font-lock-builtin-face         ((t ())))
   `(font-lock-variable-name-face   ((t ())))
   `(font-lock-type-face            ((t ())))
   '(meow-keypad-indicator          ((t (:foreground "#ab3007"))))
   '(meow-insert-indicator          ((t (:foreground "#257d22"))))
   '(meow-normal-indicator          ((t (:foreground "#713da6"))))
   '(meow-motion-indicator          ((t (:foreground "#1853cc"))))
   '(meow-keypad-cursor             ((t (:background "#ab3007"))))
   '(meow-insert-cursor             ((t (:background "#257d22"))))
   '(meow-normal-cursor             ((t (:background "#713da6"))))
   '(meow-motion-cursor             ((t (:background "#1853cc"))))
   '(dired-directory                ((t (:bold t))))
   '(highlight-symbol-face          ((t ())))
   '(yascroll:thumb-fringe          ((t (:background "black" :foreground "black"))))
   '(yascroll:thumb-text-area       ((t (:background "black" :foreground "black"))))))

(and load-file-name
     (boundp 'custom-theme-load-path)
     (add-to-list 'custom-theme-load-path
                  (file-name-as-directory
                   (file-name-directory load-file-name))))

(provide-theme 'storybook)
;;; storybook.el ends here
