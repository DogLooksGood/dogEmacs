;;; matrix-theme.el --- A minimal Dark/Green theme  -*- lexical-binding: t; -*-

;; Author: Shi Tianshu
;; Keywords: theme
;; Package-Requires: ((emacs "28.0.50"))
;; Version: 1.0.2

;;
;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.


;;; Commentary:

;;; This is a minimal A Dark/Green theme

;;; Code:

(deftheme matrix "A minimal light theme.")

(defvar matrix-theme-header-scales '(1.4 1.4 1.2 1.0 1.0 1.0 1.0)
  "Scales for headers.")

(let ((bg "#101010")
      (fg "#02a1f7")
      (cm "#1e79aa")
      (ss "#2f4177")
      (hl "#5a6873")
      (lh "#14275e")
      (fn "#283438")
      (cur "#d6e0ff"))
  (custom-theme-set-faces
   `matrix
   ;; We don't specify default foreground/background in TTY.
   `(default                        ((t :background ,bg :foreground ,fg)))
   ;; Basics
   `(cursor                         ((t (:background ,cur))))
   `(region                         ((t (:background ,lh))))
   `(hl-line                        ((t)))
   `(fringe                         ((t (:background ,bg))))
   `(show-paren-match               ((t (:box (:line-width (-1 . -1))))))
   `(highlight                      ((t (:inverse-video t))))
   `(button                         ((t (:box (:line-width (-1 . -1))))))
   `(vertical-border                ((t ())))
   `(window-divider                 ((t (:foreground "black"))))
   `(window-divider-first-pixel     ((t (:foreground ,ss))))
   `(window-divider-last-pixel      ((t (:foreground ,ss))))
   `(line-number                    ((t (:foreground ,cm))))
   `(line-number-current-line       ((t (:foreground ,fg))))
   `(parenthesis                    ((t (:foreground ,fg))))
   `(completions-common-part        ((t ())))
   `(minibuffer-prompt              ((t ())))
   `(lazy-highlight                 ((t (:background ,lh))))
   `(compilation-info               ((t ())))
   `(compilation-warning            ((t ())))
   `(warning                        ((t ())))
   `(match                          ((t (:inverse-video t))))
   `(secondary-selection            ((t (:background ,ss))))
   `(help-key-binding               ((t ())))
   `(shadow                         ((t (:foreground ,cm))))

   ;; ISearch
   `(isearch                        ((t (:inverse-video t))))
   `(isearch-fail                   ((t (:inverse-video t))))

   ;; Font Locks
   `(font-lock-comment-face         ((t (:foreground ,cm :italic t))))
   `(font-lock-comment-delimiter-face         ((t (:foreground ,cm :italic t))))
   `(font-lock-string-face          ((t (:foreground ,cm))))
   `(font-lock-doc-face             ((t (:foreground ,cm))))
   `(font-lock-builtin-face         ((t ())))
   `(font-lock-type-face            ((t ())))
   `(font-lock-variable-name-face   ((t ())))
   `(font-lock-keyword-face         ((t (:bold t))))
   `(font-lock-constant-face        ((t (:italic t))))
   `(font-lock-function-name-face   ((t (:background ,fn))))
   `(font-lock-warning-face         ((t ())))
   `(font-lock-preprocessor-face    ((t ())))

   ;; shell
   `(sh-quoted-exec                 ((t ())))

   ;; IMenu
   `(imenu-list-entry-face-0          ((t ())))
   `(imenu-list-entry-subalist-face-0 ((t (:bold t))))

   ;; Mode Line
   `(tab-line                       ((t ())))
   `(mode-line                      ((t (:foreground ,bg :background ,fg))))
   `(mode-line-inactive             ((t (:foreground ,bg :background ,cm))))
   `(header-line                      ((t ())))
   `(header-line-inactive             ((t ())))

   ;; Company
   `(company-tooltip-common         ((t ())))
   `(company-tooltip-common-selection ((t (:bold t))))
   `(company-tooltip                ((t (:background ,hl))))
   `(company-tooltip-search         ((t ())))
   `(company-tooltip-selection      ((t (:inverse-video t))))
   `(company-tooltip-annotation     ((t ())))
   `(company-scrollbar-bg           ((t (:background ,hl))))
   `(company-scrollbar-fg           ((t (:background ,fg))))
   `(company-template-field         ((t (:inherit yas-field-highlight-face))))

   ;; Yasnippet
   `(yas-field-highlight-face       ((t (:background ,hl))))

   ;; Meow
   `(meow-keypad-indicator          ((t (:bold t))))
   `(meow-insert-indicator          ((t (:bold t))))
   `(meow-normal-indicator          ((t (:bold t))))
   `(meow-motion-indicator          ((t (:bold t))))
   `(meow-bmacro-indicator          ((t (:bold t))))
   `(meow-keypad-cursor             ((t (:background ,cur))))
   `(meow-insert-cursor             ((t (:background ,cur))))
   `(meow-normal-cursor             ((t (:background ,cur))))
   `(meow-motion-cursor             ((t (:background ,cur))))

   ;; colorful paren
   `(colorful-round           ((t (:foreground ,cm))))
   `(colorful-square          ((t ())))
   `(colorful-curly           ((t ())))

   ;; Cider
   `(cider-result-overlay-face      ((t (:inverse-video t))))
   `(cider-repl-stderr-face         ((t (:bold t))))
   `(cider-repl-stdout-face         ((t ())))

   ;; Clojure
   `(clojure-character-face         ((t ())))

   ;; Magit
   ;; `(magit-diff-file-heading-highlight ((t (:background ,bg+1))))
   `(magit-branch-current              ((t (:box t :background ,hl))))
   `(magit-branch-local                ((t (:background ,hl))))
   `(magit-branch-remote               ((t (:background ,cm :foreground ,bg))))
   `(magit-branch-remote-head          ((t (:box t :background ,cm :foreground ,bg))))
   `(magit-header-line                 ((t (:bold t))))
   `(magit-head                        ((t ())))
   `(magit-section-highlight           ((t (:background ,hl))))
   `(magit-section-heading             ((t (:bold t))))
   `(magit-section-selection           ((t (:bold t))))
   `(magit-diff-hunk-heading-highlight ((t (:inverse-video t))))
   `(magit-diff-hunk-heading ((t (:foreground ,cm))))
   `(magit-diff-removed             ((t ())))
   `(magit-diff-added               ((t ())))
   `(magit-diff-removed-highlight   ((t (:background ,hl))))
   `(magit-diff-added-highlight     ((t (:background ,cm :foreground ,bg))))
   `(magit-diff-highlight           ((t ())))
   `(magit-diff-context-highlight   ((t ())))
   `(magit-hash                     ((t (:foreground ,cm))))

   ;;
   ;; ;; SMerge
   ;; `(smerge-refined-added           ((t (:background "#253325"))))
   ;; `(smerge-lower                   ((t (:background "#173017"))))
   ;;
   ;; Diff-hl
   `(diff-hl-insert                 ((t (:foreground ,cm :background ,cm))))
   `(diff-hl-change                 ((t (:foreground ,hl :background ,hl))))
   `(diff-hl-delete                 ((t (:foreground ,fg :background ,fg))))

   `(eshell-prompt                  ((t (:bold t))))
   ;;
   ;; ;; Term
   ;; `(term-color-blue                ((t (:foreground ,blue :background ,blue))))
   ;; `(term-color-green               ((t (:foreground ,green :background ,green))))
   ;; `(term-color-red                 ((t (:foreground ,red :background ,red))))
   ;;
   ;; ;; Popup
   ;; `(popup-tip-face                 ((t (:background ,bg+4 :foreground ,fg))))
   ;; `(popup-isearch-match            ((t (:background "#CFA300" :foreground "black"))))
   ;;
   ;; `(tooltip                        ((t ())))
   ;; `(dired-directory                ((t (:foreground ,light-purple))))
   ;; `(web-mode-html-attr-name-face   ((t ())))
   ;; `(web-mode-html-tag-face         ((t ())))
   ;;
   ;; Emacs Rime
   `(rime-preedit-face              ((t (:underline t))))
   `(rime-cursor-face               ((t (:inherit font-lock-constant-face))))
   `(rime-indicator-face            ((t ())))
   `(rime-indicator-dim-face        ((t ())))
   `(rime-candidate-num-face        ((t ())))
   `(rime-comment-face              ((t (:inherit font-lock-comment))))
   `(rime-code-face                 ((t (:bold t))))
   `(rime-default-face              ((t ())))
   `(rime-highlight-candidate-face  ((t ())))

   ;; sort tab
   `(sort-tab-separator-face        ((t (:foreground ,cm))))
   `(sort-tab-current-tab-face      ((t (:foreground ,fg :background ,hl))))
   `(sort-tab-other-tab-face        ((t (:foreground ,fg))))

   ;;
   ;; ;; Web Mode
   `(web-mode-function-call-face    ((t ())))
   `(web-mode-function-name-face    ((t ())))
   `(web-mode-html-tag-bracket-face ((t (:inherit parenthesis))))
   `(web-mode-symbol-face           ((t (:inherit font-lock-constant-face))))
   ;; `(css-selector                   ((t (:foreground ,purple))))
   ;;
   ;; ;; Markdown
   `(markdown-header-face-1         ((t (:bold t :height ,(nth 0 matrix-theme-header-scales)))))
   `(markdown-header-face-2         ((t (:bold t :height ,(nth 1 matrix-theme-header-scales)))))
   `(markdown-header-face-3         ((t (:bold t :height ,(nth 2 matrix-theme-header-scales)))))
   `(markdown-header-face-4         ((t (:bold t :height ,(nth 3 matrix-theme-header-scales)))))
   `(markdown-header-face-5         ((t (:bold t :height ,(nth 4 matrix-theme-header-scales)))))
   `(markdown-header-face-6         ((t (:bold t :height ,(nth 5 matrix-theme-header-scales)))))
   `(markdown-header-face-7         ((t (:bold t :height ,(nth 6 matrix-theme-header-scales)))))
   ;;
   ;; ;; Telega
   `(telega-entity-type-code        ((t ())))
   `(telega-msg-heading             ((t ())))
   `(telega-unmuted-count           ((t ())))
   ;;
   ;; ;; Org-mode
   `(org-document-title             ((t (:bold t :height ,(nth 0 matrix-theme-header-scales)))))
   `(org-link                       ((t (:underline t))))
   `(org-document-title             ((t ())))
   `(org-code                       ((t (:inherit font-lock-constant-face))))
   `(org-level-1                    ((t (:bold t :height ,(nth 0 matrix-theme-header-scales)))))
   `(org-level-2                    ((t (:bold t :height ,(nth 1 matrix-theme-header-scales)))))
   `(org-level-3                    ((t (:bold t :height ,(nth 2 matrix-theme-header-scales)))))
   `(org-level-4                    ((t (:bold t :height ,(nth 3 matrix-theme-header-scales)))))
   `(org-level-5                    ((t (:bold t :height ,(nth 4 matrix-theme-header-scales)))))
   `(org-level-6                    ((t (:bold t :height ,(nth 5 matrix-theme-header-scales)))))
   `(org-level-7                    ((t (:bold t :height ,(nth 6 matrix-theme-header-scales)))))
   ;;
   ;; ;; Treemacs
   ;; `(treemacs-root-face             ((t (:inherit font-lock-function-name-face :height 1.4 :underline t))))
   `(fill-column-indicator          ((t (:foreground ,cm))))
   `(scroll-bar                     ((t (:foreground ,fg))))
   `(sp-pair-overlay-face           ((t (:background ,hl))))
   `(sp-wrap-overlay-face           ((t (:background ,hl))))))

(and load-file-name
     (boundp 'custom-theme-load-path)
     (add-to-list 'custom-theme-load-path
                  (file-name-as-directory
                   (file-name-directory load-file-name))))

(provide-theme 'matrix)

;;; matrix-theme.el ends here
