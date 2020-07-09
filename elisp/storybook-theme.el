;;; -*- lexical-binding: t -*-

(deftheme storybook "A simple medium contrast light theme.")

(custom-theme-set-faces
 'storybook
 `(default                        ((t (:background "#f0f0f0" :foreground "#222222"))))
 `(cursor                         ((t (:background "black"))))
 '(region                         ((t (:background "#AFCFFF"))))
 `(hl-line                        ((t (:underline "#999" :overline "#999"))))
 `(font-lock-comment-face         ((t (:background "#d5f2e7" :extend t))))
 `(font-lock-doc-face             ((t (:background "#d5f2e7" :extend t))))
 `(font-lock-warning-face         ((t (:foreground "red"))))
 `(font-lock-string-face          ((t (:background "#edd5da"))))
 `(font-lock-function-name-face   ((t (:bold t))))
 `(font-lock-keyword-face         ((t ())))
 `(font-lock-constant-face        ((t (:background "#dfd5f2"))))
 `(font-lock-builtin-face         ((t ())))
 `(font-lock-variable-name-face   ((t ())))
 `(font-lock-type-face            ((t ())))
 '(meow-keypad-indicator          ((t (:foreground "white" :background "#ab3007" :bold t))))
 '(meow-insert-indicator          ((t (:foreground "white" :background "#257d22" :bold t))))
 '(meow-normal-indicator          ((t (:foreground "white" :background "purple2" :bold t))))
 '(meow-motion-indicator          ((t (:foreground "white" :background "#1853cc" :bold t))))
 '(meow-keypad-cursor             ((t (:background "#ab3007"))))
 '(meow-insert-cursor             ((t (:background "#257d22"))))
 '(meow-normal-cursor             ((t (:background "purple2"))))
 '(meow-motion-cursor             ((t (:background "#1853cc"))))
 '(dired-directory                ((t (:bold t))))
 '(mode-line                      ((t (:background "#cecece" :foreground "black"))))
 '(mode-line-inactive             ((t (:background "#e2e2e2" :foreground "#303030"))))
 '(highlight-symbol-face          ((t ())))
 '(yascroll:thumb-fringe          ((t (:background "black" :foreground "black"))))
 '(yascroll:thumb-text-area       ((t (:background "black" :foreground "black"))))
 '(rime-indicator-face            ((t (:foreground "#00ffff"))))
 '(rime-indicator-dim-face        ((t ())))
 '(rime-default-face              ((t (:foreground "#303030" :background "#ececec"))))
 '(telega-entity-type-code        ((t (:inherit fixed-pitch))))
 '(telega-entity-type-pre         ((t (:inherit fixed-pitch))))
 '(cider-fringe-good-face         ((t (:foreground "#006666"))))
 '(fringe                         ((t (:background "#e9e9e9")))))

(and load-file-name
     (boundp 'custom-theme-load-path)
     (add-to-list 'custom-theme-load-path
                  (file-name-as-directory
                   (file-name-directory load-file-name))))

(provide-theme 'storybook)
;;; storybook.el ends here
