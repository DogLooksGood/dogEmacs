;;; -*- lexical-binding: t -*-

(deftheme storybook "A simple medium contrast light theme.")

(defvar storybook-use-italic t
  "Non-nil means use italic for comment and docstring.")

(defvar storybook-header-scales '(2.2 1.8 1.6 1.4 1.2 1.2 1.2)
  "Scales for headers.")

(let ((fg "#442C09")
      (fg+1 "#806040")
      (hl "#EDCCB7")
      (bg "#FCF7E8")
      (bg-1 "#F2E9D0")
      (err "#8E1316")
      (green-bg "#E1F0E5")
      (green-fg "#0A3A45")
      (red-bg "#FAECE1")
      (purple-bg "#E9DEFA")
      (red-fg "#5E1477")
      (hl2 "#cbd8b6")
      (sec-sel "#CABAE0"))
  (custom-theme-set-faces
   `storybook
   `(default                        ((((type tty)) :background ,bg)
                                     (((type graphic)) :background ,bg :foreground ,fg)))
   `(hl-line                        ((((type graphic)) :overline ,hl :underline ,hl)
                                     (((type tty)) :background "#ffffff")))
   `(cursor                         ((t (:background "black"))))
   `(region                         ((t (:background ,hl))))
   `(fringe                         ((t ())))
   `(fill-column-indicator          ((t (:foreground ,bg-1))))
   `(font-lock-comment-delimiter-face ((t (:background ,green-bg :foreground ,fg+1 :italic t))))
   `(font-lock-comment-face         ((t (:background ,green-bg :extend t :italic t))))
   `(font-lock-doc-face             ((t (:background ,green-bg :extend t :italic t))))
   `(font-lock-warning-face         ((t (:foreground ,err))))
   `(font-lock-string-face          ((t (:background ,red-bg))))
   `(font-lock-function-name-face   ((t (:background ,purple-bg))))
   `(font-lock-keyword-face         ((t (:foreground ,green-fg))))
   `(font-lock-constant-face        ((t (:foreground ,red-fg))))
   `(font-lock-builtin-face         ((t ())))
   `(font-lock-variable-name-face   ((t ())))
   `(font-lock-type-face            ((t ())))
   `(font-lock-preprocessor-face    ((t (:inherit font-lock-constant-face))))
   `(compilation-warning            ((t (:inherit font-lock-warning-face))))
   `(warning                        ((t (:inherit font-lock-warning-face))))
   `(meow-keypad-indicator          ((t (:foreground "#801717" :background "#FF6666"))))
   `(meow-insert-indicator          ((t (:foreground "#309030" :background "#AAE9A0"))))
   `(meow-normal-indicator          ((t (:foreground "#6F5033" :background "#FFEE99"))))
   `(meow-motion-indicator          ((t (:foreground "#505090" :background "#AACCEE"))))
   `(meow-keypad-cursor             ((t ())))
   `(meow-insert-cursor             ((t ())))
   `(meow-normal-cursor             ((t ())))
   `(meow-motion-cursor             ((t ())))
   `(meow-grab                      ((t (:background ,hl2))))
   `(dired-directory                ((t (:inherit font-lock-keyword-face))))
   `(mode-line                      ((((type tty))
                                      (:background "grey70"))
                                     (((type graphic))
                                      (:box (:line-width (-1 . -1) :color "#a8a296") :background "#ede4d1"))))
   `(mode-line-inactive             ((((type tty))
                                      (:background "grey80"))
                                     (((type graphic))
                                      (:box (:line-width (-1 . -1) :color "#a8a296") :background "#e0d8c5"))))
   `(parenthesis                    ((t (:foreground "#909090"))))
   `(highlight-symbol-face          ((t ())))
   `(yascroll:thumb-fringe          ((t (:background "#606060" :foreground "#606060"))))
   `(yascroll:thumb-text-area       ((t (:background "#606060" :foreground "#606060"))))
   `(rime-indicator-face            ((t (:foreground "#1853cc"))))
   `(rime-indicator-dim-face        ((t ())))
   `(rime-default-face              ((t (:foreground "#303030"))))
   `(rime-preedit-face              ((t (:inverse-video nil :underline t))))
   `(rime-cursor-face               ((t (:inherit font-lock-constant-face))))
   `(rime-candidate-num-face        ((t ())))
   `(rime-highlight-candidate-face  ((t (:background "#ede4d1"))))
   `(rime-comment-face              ((t ())))
   `(telega-msg-heading             ((t (:background "#ede4d1"))))
   `(telega-entity-type-code        ((t (:inherit fixed-pitch))))
   `(telega-entity-type-pre         ((t (:inherit fixed-pitch))))
   `(cider-fringe-good-face         ((t (:foreground "#006666"))))
   `(web-mode-html-attr-name-face   ((t ())))
   `(web-mode-html-tag-face         ((t ())))
   `(line-number                    ((t :foreground ,fg+1)))
   `(line-number-current-line       ((((type graphic))
                                      :overline ,hl
                                      :underline ,hl
                                      :bold t
                                      :foreground "#404040"
                                      :inherit default)
                                     (t (:bold t :inherit default :background "#ffffff"))))
   `(mc/cursor-bar-face             ((t (:background "grey50" :height 1))))
   `(dired-subtree-depth-1-face     ((t (:background "grey90"))))
   `(dired-subtree-depth-2-face     ((t (:background "grey80"))))
   `(dired-subtree-depth-3-face     ((t (:background "grey70"))))
   `(dired-subtree-depth-4-face     ((t (:background "grey90"))))
   `(dired-subtree-depth-5-face     ((t (:background "grey80"))))
   `(dired-subtree-depth-6-face     ((t (:background "grey70"))))
   `(isearch                        ((t (:background "#ff99a5"))))
   `(ivy-current-match              ((t (:background "#EAC5A0"))))
   `(ivy-minibuffer-match-highlight ((t (:foreground "#00D7D7"))))
   `(ivy-minibuffer-match-face-1    ((t ())))
   `(ivy-minibuffer-match-face-2    ((t ())))
   `(ivy-minibuffer-match-face-3    ((t ())))
   `(ivy-minibuffer-match-face-4    ((t ())))
   `(minibuffer-prompt              ((t (:foreground "#0a3a45"))))
   `(web-mode-function-call-face    ((t ())))
   `(web-mode-function-name-face    ((t ())))
   `(web-mode-html-tag-face         ((t (:bold t))))
   `(window-divider                 ((t (:foreground "grey60"))))
   `(vertical-border                ((t (:foreground "grey20"))))
   `(web-mode-html-tag-bracket-face ((t (:inherit parenthesis))))
   `(web-mode-doctype-face          ((t (:foreground "grey40"))))
   `(web-mode-symbol-face           ((t (:inherit font-lock-constant-face))))
   `(css-selector                   ((t (:inherit font-lock-constant-face))))
   `(markdown-header-face-1         ((t (:bold t :height 2.2))))
   `(markdown-header-face-2         ((t (:bold t :height 1.8))))
   `(markdown-header-face-3         ((t (:bold t :height 1.4))))
   `(markdown-header-face-4         ((t (:bold t :height 1.2))))
   `(markdown-header-face-5         ((t (:bold t :height 1.2))))
   `(markdown-header-face-6         ((t (:bold t :height 1.2))))
   `(markdown-header-face-7         ((t (:bold t :height 1.2))))
   `(org-document-title  ((t (:foreground ,fg :height 2.2))))
   `(org-block           ((t (:background "#f2ecd7" :extend t))))
   `(org-meta-line       ((t (:background "#e5dfc9" :extend t))))
   `(org-table           ((t (:foreground "grey40"))))
   `(org-level-1         ((t (:bold t :height 2.2))))
   `(org-level-2         ((t (:bold t :height 1.8))))
   `(org-level-3         ((t (:bold t :height 1.4))))
   `(org-level-4         ((t (:bold t :height 1.2))))
   `(org-level-5         ((t (:bold t :height 1.2))))
   `(org-level-6         ((t (:bold t :height 1.2))))
   `(org-level-7         ((t (:bold t :height 1.2))))
   `(secondary-selection ((t (:background ,sec-sel))))
   `(header-line         ((t (:background "#ede4d1" :foreground "#442c09" :underline "#a8a296"))))))

(and load-file-name
     (boundp 'custom-theme-load-path)
     (add-to-list 'custom-theme-load-path
                  (file-name-as-directory
                   (file-name-directory load-file-name))))

(provide-theme 'storybook)
;;; storybook-theme.el ends here
