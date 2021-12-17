;;; init-theme.el --- Theme helpers -*- lexical-binding: t -*-

(defvar +theme-header-scales
  '(1.4 1.4 1.2 1.0 1.0 1.0 1.0)
  "Scales for headers.")

(defmacro +make-theme (theme doc &rest colors)
  "A helper macro to quickly define a theme.

Borrow the ideas from nano-theme.

Argument THEME symbol for theme name.
Argument DOC docstring.
Argument COLORS items in plist format, following keys are required.

* c
  The color of cursors.

* fg
  The default foreground color.

* bg
  The default background color.

* ff
  Faded foreground for contents those require less attention.

* hb
  Highlight background for indicators.

* sb
  Subte background for special areas.
"
  (let* ((c (plist-get colors :c))
         (fg (plist-get colors :fg))
         (bg (plist-get colors :bg))
         (ff (plist-get colors :ff))
         (hb (plist-get colors :hb))
         (sb (plist-get colors :sb)))
    `(progn
       (deftheme ,theme ,doc)

       (custom-theme-set-faces
        (quote ,theme)
        '(default                        ((t :foreground ,fg :background ,bg)))
        '(cursor                         ((t :background ,c)))
        '(hl-line                        ((t :background ,sb)))
        '(region                         ((t :background ,hb)))
        '(fringe                         ((t :background ,sb)))
        '(highlight                      ((t :background ,hb)))

        '(show-paren-match               ((t :box (:line-width (-1 . -1)))))
        '(line-number                    ((t (:foreground ,ff))))
        '(line-number-current-line       ((t (:foreground ,fg))))
        '(parenthesis                    ((t (:foreground ,fg))))
        '(completions-common-part        ((t ())))
        '(minibuffer-prompt              ((t ())))
        '(lazy-highlight                 ((t (:foreground ,fg :box (:line-width (-1 . -1))))))
        '(compilation-info               ((t ())))
        '(compilation-warning            ((t ())))
        '(warning                        ((t ())))
        '(match                          ((t (:foreground ,fg :box (:line-width (-1 . -1))))))
        '(secondary-selection            ((t (:background ,sb))))
        '(help-key-binding               ((t ())))
        '(shadow                         ((t (:foreground ,ff))))

        ;; window divider
        '(window-divider                 ((t (:foreground ,hb))))
        '(window-divider-first-pixel     ((t (:foreground ,hb))))
        '(window-divider-last-pixel      ((t (:foreground ,hb))))

        ;; ISearch
        '(isearch                        ((t (:inverse-video t))))
        '(isearch-fail                   ((t (:inverse-video t))))

        ;; Font Locks
        '(font-lock-comment-face         ((t (:foreground ,ff :italic t))))
        '(font-lock-comment-delimiter-face ((t (:foreground ,ff :italic t))))
        '(font-lock-string-face          ((t (:foreground ,ff))))
        '(font-lock-doc-face             ((t (:foreground ,ff))))
        '(font-lock-builtin-face         ((t ())))
        '(font-lock-type-face            ((t ())))
        '(font-lock-variable-name-face   ((t ())))
        '(font-lock-keyword-face         ((t (:bold t))))
        '(font-lock-constant-face        ((t (:italic t))))
        '(font-lock-function-name-face   ((t (:background ,sb))))
        '(font-lock-warning-face         ((t ())))
        '(font-lock-preprocessor-face    ((t ())))

        ;; shell
        '(sh-quoted-exec                 ((t ())))

        ;; IMenu
        '(imenu-list-entry-face-0          ((t ())))
        '(imenu-list-entry-subalist-face-0 ((t (:bold t))))

        ;; Mode Line
        '(tab-line                       ((t ())))
        '(mode-line                      ((t (:inverse-video t))))
        '(mode-line-inactive             ((t (:background ,sb))))
        '(header-line                    ((t ())))
        '(header-line-inactive           ((t ())))

        ;; Company
        '(company-tooltip-common         ((t ())))
        '(company-tooltip-common-selection ((t (:bold t))))
        '(company-tooltip                ((t (:background ,sb))))
        '(company-tooltip-search         ((t ())))
        '(company-tooltip-selection      ((t (:inverse-video t))))
        '(company-tooltip-annotation     ((t ())))
        '(company-scrollbar-bg           ((t (:background ,bg))))
        '(company-scrollbar-fg           ((t (:background ,fg))))
        '(company-template-field         ((t (:inherit yas-field-highlight-face))))

        ;; Yasnippet
        '(yas-field-highlight-face       ((t (:background ,hb))))

        ;; Meow
        '(meow-keypad-indicator          ((t (:bold t))))
        '(meow-insert-indicator          ((t (:bold t))))
        '(meow-normal-indicator          ((t (:bold t))))
        '(meow-motion-indicator          ((t (:bold t))))
        '(meow-beacon-indicator          ((t (:bold t))))

        ;; colorful paren
        '(colorful-round           ((t (:foreground ,ff))))
        '(colorful-square          ((t ())))
        '(colorful-curly           ((t ())))

        ;; Cider
        '(cider-result-overlay-face      ((t (:inverse-video t))))
        '(cider-repl-stderr-face         ((t (:bold t))))
        '(cider-repl-stdout-face         ((t ())))

        ;; Clojure
        '(clojure-character-face         ((t ())))

        ;; Magit
        ;; '(magit-diff-file-heading-highlight ((t (:background ,bg+1))))
        '(magit-branch-current              ((t (:box t :background ,hb))))
        '(magit-branch-local                ((t (:background ,hb))))
        '(magit-branch-remote               ((t (:background ,ff :foreground ,bg))))
        '(magit-branch-remote-head          ((t (:box t :background ,ff :foreground ,bg))))
        '(magit-header-line                 ((t (:bold t))))
        '(magit-head                        ((t ())))
        '(magit-section-highlight           ((t (:background ,hb))))
        '(magit-section-heading             ((t (:bold t))))
        '(magit-section-selection           ((t (:bold t))))
        '(magit-diff-hunk-heading-highlight ((t (:inverse-video t))))
        '(magit-diff-hunk-heading ((t (:foreground ,ff))))
        '(magit-diff-removed             ((t ())))
        '(magit-diff-added               ((t ())))
        '(magit-diff-removed-highlight   ((t (:background ,hb))))
        '(magit-diff-added-highlight     ((t (:background ,ff :foreground ,bg))))
        '(magit-diff-highlight           ((t ())))
        '(magit-diff-context-highlight   ((t ())))
        ;;
        ;; ;; SMerge
        ;; '(smerge-refined-added           ((t (:background "#253325"))))
        ;; '(smerge-lower                   ((t (:background "#173017"))))
        ;;
        ;; Diff-hl
        '(diff-hl-insert                 ((t (:foreground ,ff :background ,ff))))
        '(diff-hl-change                 ((t (:foreground ,hb :background ,hb))))
        '(diff-hl-delete                 ((t (:foreground ,fg :background ,fg))))

        '(eshell-prompt                  ((t (:bold t))))
        ;;
        ;; ;; Term
        ;; '(term-color-blue                ((t (:foreground ,blue :background ,blue))))
        ;; '(term-color-green               ((t (:foreground ,green :background ,green))))
        ;; '(term-color-red                 ((t (:foreground ,red :background ,red))))
        ;;
        ;; ;; Popup
        ;; '(popup-tip-face                 ((t (:background ,bg+4 :foreground ,fg))))
        ;; '(popup-isearch-match            ((t (:background "#CFA300" :foreground "black"))))
        ;;
        '(tooltip                        ((t ())))
        '(dired-directory                ((t (:bold t))))
        '(web-mode-html-attr-name-face   ((t ())))
        '(web-mode-html-tag-face         ((t ())))

        ;; Emacs Rime
        '(rime-preedit-face              ((t (:underline t))))
        '(rime-cursor-face               ((t (:inherit font-lock-constant-face))))
        '(rime-indicator-face            ((t ())))
        '(rime-indicator-dim-face        ((t ())))
        '(rime-candidate-num-face        ((t ())))
        '(rime-comment-face              ((t (:inherit font-lock-comment))))
        '(rime-code-face                 ((t (:bold t))))
        '(rime-default-face              ((t ())))
        '(rime-highlight-candidate-face  ((t ())))

        ;;
        ;; ;; Web Mode
        '(web-mode-function-call-face    ((t ())))
        '(web-mode-function-name-face    ((t ())))
        '(web-mode-html-tag-bracket-face ((t (:inherit parenthesis))))
        '(web-mode-symbol-face           ((t (:inherit font-lock-constant-face))))
        '(web-mode-doctype-face          ((t (:inherit shadow))))
        ;; '(css-selector                   ((t (:foreground ,purple))))
        ;;
        ;; ;; Markdown
        '(markdown-header-face-1         ((t (:bold t :height ,(nth 0 +theme-header-scales)))))
        '(markdown-header-face-2         ((t (:bold t :height ,(nth 1 +theme-header-scales)))))
        '(markdown-header-face-3         ((t (:bold t :height ,(nth 2 +theme-header-scales)))))
        '(markdown-header-face-4         ((t (:bold t :height ,(nth 3 +theme-header-scales)))))
        '(markdown-header-face-5         ((t (:bold t :height ,(nth 4 +theme-header-scales)))))
        '(markdown-header-face-6         ((t (:bold t :height ,(nth 5 +theme-header-scales)))))
        '(markdown-header-face-7         ((t (:bold t :height ,(nth 6 +theme-header-scales)))))
        ;;
        ;; ;; Telega
        '(telega-entity-type-code        ((t ())))
        '(telega-msg-heading             ((t ())))
        '(telega-unmuted-count           ((t ())))
        ;;
        ;; ;; Org-mode
        '(org-document-title             ((t (:bold t :height ,(nth 0 +theme-header-scales)))))
        '(org-link                       ((t (:underline t))))
        '(org-document-title             ((t ())))
        '(org-code                       ((t (:inherit font-lock-constant-face))))
        '(org-level-1                    ((t (:bold t :height ,(nth 0 +theme-header-scales)))))
        '(org-level-2                    ((t (:bold t :height ,(nth 1 +theme-header-scales)))))
        '(org-level-3                    ((t (:bold t :height ,(nth 2 +theme-header-scales)))))
        '(org-level-4                    ((t (:bold t :height ,(nth 3 +theme-header-scales)))))
        '(org-level-5                    ((t (:bold t :height ,(nth 4 +theme-header-scales)))))
        '(org-level-6                    ((t (:bold t :height ,(nth 5 +theme-header-scales)))))
        '(org-level-7                    ((t (:bold t :height ,(nth 6 +theme-header-scales)))))
        '(org-todo                       ((t (:bold t))))
        '(org-done                       ((t (:bold t :inherit shadow))))
        '(org-headline-done              ((t (:inherit shadow))))
        '(org-drawer                     ((t (:foreground ,ff))))

        ;; ;; Treemacs
        '(fill-column-indicator          ((t (:foreground ,ff))))
        '(scroll-bar                     ((t (:foreground ,fg))))
        '(sp-pair-overlay-face           ((t (:background ,hb))))
        '(sp-wrap-overlay-face           ((t (:background ,hb))))

        '(flycheck-warning ((t (:underline (:style wave :color ,ff)))))
        '(flycheck-error   ((t (:underline (:style wave :color ,fg)))))
        '(flymake-warning  ((t (:underline (:style wave :color ,ff)))))
        '(flymake-error    ((t (:underline (:style wave :color ,fg)))))
        '(flymake-note     ((t (:underline (:style wave :color ,hb)))))

        '(tab-bar                        ((t (:background ,sb))))
        '(tab-bar-tab-group-current      ((t ())))
        '(tab-bar-tab                    ((t (:bold t :background ,hb))))
        '(tab-bar-tab-group-inactive     ((t ())))
        '(tab-bar-tab-inactive           ((t (:bold nil :inherit shadow))))

        '(yascroll:thumb-fringe          ((t (:foreground ,ff :background ,ff))))
        '(yascroll:thumb-text-area       ((t (:foreground ,ff :background ,ff)))))

       (and load-file-name
            (boundp 'custom-theme-load-path)
            (add-to-list 'custom-theme-load-path
                         (file-name-as-directory
                          (file-name-directory load-file-name)))))))

(provide 'init-theme)
