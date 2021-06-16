;;; minidark-theme.el --- A minimal light theme  -*- lexical-binding: t; -*-

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

;;; This is a minimal light theme.

;;; Code:

(deftheme minidark "A minimal light theme.")

(defvar minidark-theme-header-scales '(1.2 1.2 1.2 1.0 1.0 1.0 1.0)
  "Scales for headers.")

(let ((bg "#000000")
      (fg "#A2A2A2")
      (cm "#707070")
      (hl "#303030")
      (kw "#A2A2A2")
      (fn "#F0F0F0")
      (st "#5bd98b")
      (str "#4490bd")
      (doc "#cc6666")
      (cur "#FFFFFF")
      (pop "#202020"))
  (custom-theme-set-faces
   `minidark
   ;; We don't specify default foreground/background in TTY.
   `(default                        ((t :background ,bg :foreground ,fg)))
   ;; Basics
   `(cursor                         ((t (:background ,cur))))
   `(region                         ((t (:background ,hl))))
   `(hl-line                        ((t)))
   `(fringe                         ((t (:background ,bg))))
   `(show-paren-match               ((t (:box (:line-width (-1 . -1))))))
   `(highlight                      ((t (:inverse-video t))))
   `(button                         ((t (:box (:line-width (-1 . -1))))))
   `(vertical-border                ((t ())))
   `(window-divider                 ((t (:foreground ,cm))))
   `(window-divider-first-pixel     ((t (:foreground ,cm))))
   `(window-divider-last-pixel      ((t (:foreground ,cm))))
   `(line-number                    ((t (:foreground ,cm))))
   `(line-number-current-line       ((t (:foreground ,fg))))
   `(completions-common-part        ((t ())))
   `(minibuffer-prompt              ((t ())))
   `(lazy-highlight                 ((t (:background ,hl))))
   `(compilation-info               ((t ())))
   `(compilation-warning            ((t ())))
   `(warning                        ((t ())))
   `(match                          ((t (:inverse-video t))))
   `(secondary-selection            ((t (:inverse-video t :foreground ,cm))))
   `(help-key-binding               ((t ())))

   ;; ISearch
   `(isearch                        ((t (:inverse-video t))))
   `(isearch-fail                   ((t (:inverse-video t))))

   ;; Font Locks
   `(font-lock-comment-face         ((t (:foreground ,doc))))
   `(font-lock-comment-delimiter-face  ((t (:inherit font-lock-comment-face))))
   `(font-lock-string-face          ((t (:foreground ,str))))
   `(font-lock-doc-face             ((t (:foreground ,doc))))
   `(font-lock-builtin-face         ((t ())))
   `(font-lock-type-face            ((t ())))
   `(font-lock-variable-name-face   ((t ())))
   `(font-lock-keyword-face         ((t (:foreground ,kw))))
   `(font-lock-constant-face        ((t (:foreground ,st))))
   `(font-lock-function-name-face   ((t (:foreground ,fn))))
   `(font-lock-warning-face         ((t ())))
   `(font-lock-preprocessor-face    ((t ())))

   `(selectrum-current-candidate    ((t (:foreground ,st))))
   `(selectrum-prescient-primary-highlight ((t (:underline t))))

   ;; shell
   `(sh-quoted-exec                 ((t ())))

   ;; IMenu
   `(imenu-list-entry-face-0          ((t ())))
   `(imenu-list-entry-subalist-face-0 ((t (:bold t))))

   ;; Mode Line
   `(mode-line                      ((t (:inverse-video t))))
   `(mode-line-inactive             ((t (:background ,hl))))
   `(header-line                    ((t (:inverse-video t))))
   `(header-line-inactive           ((t (:background ,hl))))

   ;; Company
   `(company-tooltip-common         ((t ())))
   `(company-tooltip-common-selection ((t ())))
   `(company-tooltip                ((t (:background ,hl))))
   `(company-tooltip-search         ((t ())))
   `(company-tooltip-selection      ((t (:inverse-video t))))
   `(company-tooltip-annotation     ((t ())))
   `(company-scrollbar-bg           ((t (:background ,bg))))
   `(company-scrollbar-fg           ((t (:background ,fg))))
   `(company-template-field         ((t (:inherit yas-field-highlight-face))))

   ;; Yasnippet
   `(yas-field-highlight-face       ((t (:background ,hl))))

   ;; Meow
   `(meow-keypad-indicator          ((t ())))
   `(meow-insert-indicator          ((t ())))
   `(meow-normal-indicator          ((t ())))
   `(meow-motion-indicator          ((t ())))
   `(meow-keypad-cursor             ((t (:background "red"))))
   `(meow-insert-cursor             ((t (:background "cyan"))))
   `(meow-normal-cursor             ((t (:background ,cur))))
   `(meow-motion-cursor             ((t (:background ,cur))))

   ;; Cider
   `(cider-result-overlay-face      ((t (:inverse-video t))))
   `(cider-repl-stderr-face         ((t (:foreground "red"))))
   `(cider-repl-stdout-face         ((t ())))

   ;; Clojure
   `(clojure-character-face         ((t ())))

   ;; Magit
   ;; `(magit-diff-file-heading-highlight ((t (:background ,bg+1))))
   `(magit-header-line                 ((t ())))
   `(magit-head                        ((t ())))
   `(magit-section-highlight           ((t (:background ,hl))))
   `(magit-section-heading             ((t ())))
   `(magit-section-selection           ((t ())))
   ;; `(magit-diff-removed             ((t (:inherit font-lock-string-face))))
   ;; `(magit-diff-added               ((t (:inherit font-lock-comment-face))))
   ;; `(magit-diff-removed-highlight   ((t (:inherit font-lock-string-face :background ,bg+2))))
   ;; `(magit-diff-added-highlight     ((t (:inherit font-lock-comment-face :background ,bg+2))))
   ;; `(magit-diff-highlight           ((t (:background ,bg+1))))
   ;; `(magit-diff-context-highlight   ((t (:background ,bg+1))))
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
   `(rime-comment-face              ((t ())))
   `(rime-code-face                 ((t (:bold t))))
   `(rime-default-face              ((t ())))
   `(rime-highlight-candidate-face  ((t ())))

   ;;
   ;; ;; Web Mode
   ;; `(web-mode-function-call-face    ((t ())))
   ;; `(web-mode-function-name-face    ((t ())))
   ;; `(web-mode-html-tag-bracket-face ((t (:inherit parenthesis))))
   ;; `(web-mode-symbol-face           ((t (:foreground ,purple))))
   ;; `(css-selector                   ((t (:foreground ,purple))))
   ;;
   ;; ;; Markdown
   ;; `(markdown-header-face-1         ((t (:bold t :height ,(nth 0 minidark-theme-header-scales)))))
   ;; `(markdown-header-face-2         ((t (:bold t :height ,(nth 1 minidark-theme-header-scales)))))
   ;; `(markdown-header-face-3         ((t (:bold t :height ,(nth 2 minidark-theme-header-scales)))))
   ;; `(markdown-header-face-4         ((t (:bold t :height ,(nth 3 minidark-theme-header-scales)))))
   ;; `(markdown-header-face-5         ((t (:bold t :height ,(nth 4 minidark-theme-header-scales)))))
   ;; `(markdown-header-face-6         ((t (:bold t :height ,(nth 5 minidark-theme-header-scales)))))
   ;; `(markdown-header-face-7         ((t (:bold t :height ,(nth 6 minidark-theme-header-scales)))))
   ;;
   ;; ;; Telega
   `(telega-entity-type-code        ((t ())))
   `(telega-msg-heading             ((t ())))
   `(telega-unmuted-count           ((t ())))
   ;;
   ;; ;; Org-mode
   `(org-document-title             ((t (:bold t :height ,(nth 0 minidark-theme-header-scales)))))
   `(org-link                       ((t (:underline t))))
   `(org-document-title             ((t ())))
   `(org-code                       ((t (:inherit font-lock-constant-face))))
   `(org-level-1                    ((t (:bold t :height ,(nth 0 minidark-theme-header-scales)))))
   `(org-level-2                    ((t (:bold t :height ,(nth 1 minidark-theme-header-scales)))))
   `(org-level-3                    ((t (:bold t :height ,(nth 2 minidark-theme-header-scales)))))
   `(org-level-4                    ((t (:bold t :height ,(nth 3 minidark-theme-header-scales)))))
   `(org-level-5                    ((t (:bold t :height ,(nth 4 minidark-theme-header-scales)))))
   `(org-level-6                    ((t (:bold t :height ,(nth 5 minidark-theme-header-scales)))))
   `(org-level-7                    ((t (:bold t :height ,(nth 6 minidark-theme-header-scales)))))
   ;;
   ;; ;; Treemacs
   ;; `(treemacs-root-face             ((t (:inherit font-lock-function-name-face :height 1.4 :underline t))))
   `(fill-column-indicator          ((t (:foreground ,cm))))
   `(scroll-bar                     ((t (:foreground ,fg))))
   `(parenthesis                    ((t (:foreground ,cm))))
   `(eldoc-box-body                 ((t (:background ,pop :inherit variable-pitch))))

   `(wgrep-face                     ((t (:underline ,st))))))

(and load-file-name
     (boundp 'custom-theme-load-path)
     (add-to-list 'custom-theme-load-path
                  (file-name-as-directory
                   (file-name-directory load-file-name))))

(provide-theme 'minidark)

;;; minidark-theme.el ends here
