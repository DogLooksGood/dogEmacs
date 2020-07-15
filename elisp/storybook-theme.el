;;; -*- lexical-binding: t -*-

(deftheme storybook "A simple medium contrast light theme.")

(custom-theme-set-faces
 'storybook
 `(default                        ((t (:background "#f0f0f0" :foreground "#222222"))))
 `(cursor                         ((t (:background "black"))))
 '(region                         ((t (:background "#AFCFFF"))))
 `(hl-line                        ((t ())))
 `(font-lock-comment-face         ((t (:background "#f2dae7"))))
 `(font-lock-doc-face             ((t (:background "#f2dae7"))))
 `(font-lock-warning-face         ((t (:background "dark red" :foreground "white"))))
 `(font-lock-string-face          ((t (:background "#c5eae0"))))
 `(font-lock-function-name-face   ((t (:bold t))))
 `(font-lock-keyword-face         ((t (:foreground "black"))))
 `(font-lock-constant-face        ((t (:foreground "#5c14ad"))))
 `(font-lock-builtin-face         ((t ())))
 `(font-lock-variable-name-face   ((t ())))
 `(font-lock-type-face            ((t ())))
 '(meow-keypad-indicator          ((t (:foreground "white" :background "#ab3007" :bold t))))
 '(meow-insert-indicator          ((t (:foreground "white" :background "green4" :bold t))))
 '(meow-normal-indicator          ((t (:foreground "white" :background "purple4" :bold t))))
 '(meow-motion-indicator          ((t (:foreground "white" :background "#1853cc" :bold t))))
 '(meow-keypad-cursor             ((t (:background "black"))))
 '(meow-insert-cursor             ((t (:background "black"))))
 '(meow-normal-cursor             ((t (:background "black"))))
 '(meow-motion-cursor             ((t (:background "black"))))
 '(dired-directory                ((t (:bold t))))
 '(mode-line                      ((t (:background "#cecece" :foreground "black"))))
 '(mode-line-inactive             ((t (:background "#e2e2e2" :foreground "#404040"))))
 '(highlight-symbol-face          ((t ())))
 '(yascroll:thumb-fringe          ((t (:background "black" :foreground "black"))))
 '(yascroll:thumb-text-area       ((t (:background "black" :foreground "black"))))
 '(rime-indicator-face            ((t (:bold t :foreground "#1853cc"))))
 '(rime-indicator-dim-face        ((t ())))
 '(rime-default-face              ((t (:foreground "#303030" :background "#ececec"))))
 '(rime-preedit-face              ((t (:inverse-video nil :underline t))))
 '(rime-cursor-face               ((t (:inherit font-lock-constant-face))))
 '(rime-candidate-num-face        ((t ())))
 '(rime-comment-face              ((t ())))
 '(rime-indicator-face            ((t (:foreground "#713da6"))))
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
