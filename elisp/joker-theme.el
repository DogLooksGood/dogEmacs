;;; -*- lexical-binding: t -*-

(deftheme joker "Minimal colortheme for terminal.")

(custom-theme-set-faces
 'joker
 '(default                        ((t (:foreground "color-248"))))
 '(mc/cursor-face                 ((t (:background "color-246" :foreground "black"))))
 '(region                         ((t (:background "color-35" :foreground "black"))))
 '(hl-line                        ((t (:background "black"))))
 '(show-paren-match               ((t (:background "color-42" :foreground "black"))))
 '(font-lock-comment-face         ((t (:foreground "color-37"))))
 '(font-lock-string-face          ((t (:foreground "color-161"))))
 '(font-lock-doc-face             ((t (:foreground "color-37"))))
 '(font-lock-builtin-face         ((t ())))
 '(font-lock-type-face            ((t ())))
 '(font-lock-variable-name-face   ((t ())))
 '(font-lock-keyword-face         ((t ())))
 '(font-lock-constant-face        ((t (:foreground "color-178"))))
 '(font-lock-function-name-face   ((t (:foreground "color-37"))))
 '(m4d-insert-indicator           ((t (:foreground "color-178"))))
 '(m4d-visual-indicator           ((t (:foreground "color-42"))))
 '(cider-result-overlay-face      ((t (:background "color-18"))))
 '(mode-line                      ((t ())))
 '(highlight                      ((t (:background "color-37" :foreground "black"))))
 '(isearch                        ((t (:background "color-41" :foreground "black"))))
 '(isearch-fail                   ((t (:backgronud "color-171" :foreground "black"))))
 '(popup-isearch-match            ((t (:background "color-178" :foreground "black"))))
 '(ivy-highlight-face             ((t (:backgronud "color-39" :foreground "black"))))
 '(ivy-yanked-word                ((t (:background "yellow" :foreground "black"))))
 '(ivy-remote                     ((t ())))
 '(counsel-outline-default        ((t ())))
 '(completions-common-part        ((t ())))
 '(minibuffer-prompt              ((t ())))
 '(lazy-highlight                 ((t (:foreground "white"))))
 '(magit-diff-file-heading-highlight ((t (:background "color-18"))))
 '(magit-section-highlight           ((t (:background "color-18"))))
 '(swiper-background-match-face-1 ((t (:background "color-41" :foreground "black"))))
 '(swiper-background-match-face-2 ((t (:background "color-41" :foreground "black"))))
 '(swiper-background-match-face-3 ((t (:background "color-41" :foreground "black"))))
 '(swiper-background-match-face-4 ((t (:background "color-41" :foreground "black"))))
 '(swiper-match-face-1            ((t (:foreground "white"))))
 '(swiper-match-face-2            ((t (:foreground "white"))))
 '(swiper-match-face-3            ((t (:foreground "white"))))
 '(swiper-match-face-4            ((t (:foreground "white"))))
 '(ivy-minibuffer-match-highlight ((t (:foreground "color-44"))))
 '(ivy-minibuffer-match-face-1    ((t (:foreground "color-247"))))
 '(ivy-minibuffer-match-face-2    ((t (:foreground "color-161"))))
 '(ivy-minibuffer-match-face-3    ((t (:foreground "color-178"))))
 '(ivy-minibuffer-match-face-4    ((t (:foreground "color-50")))))

(and load-file-name
     (boundp 'custom-theme-load-path)
     (add-to-list 'custom-theme-load-path
                  (file-name-as-directory
                   (file-name-directory load-file-name))))


(provide-theme 'joker)
