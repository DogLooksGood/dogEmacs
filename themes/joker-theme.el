;;; joker-theme.el --- A minimal dark theme  -*- lexical-binding: t; -*-

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

;;; This is a minimal dark theme.

;;; Code:

(deftheme joker "A minimal dark theme.")

(defvar joker-theme-use-italic t
  "Non-nil means use italic for comment and docstring.")

(defvar joker-theme-header-scales
  '(1.6 1.4 1.2 1.2 1.2 1.1 1.0)
  "Scales for headers.")

(defvar joker-theme-main-color "#00AAAA"
  "The main color used for some places.

You may want to set this to window's border color.")


(let ((fg "#AFAFAF")
      (fg+1 "#CCCCCC")
      (fg-1 "#909090")
      (bg "#171717")
      (bg-1 "#131313")
      (bg+1 "#242424")
      (bg+2 "#303030")
      (bg+3 "#404040")
      (bg+4 "#505050")
      (white "#E0E0E0")
      (italic joker-theme-use-italic)
      (yellow "#CFA300")
      (red "#E24C49")
      (blue "#009F9F")
      (green "#39BA7E")
      (purple "#B762DE")
      (light-purple "#B28CE2")
      (orange "#FC9F4E")
      (region "#173525")
      (region2 "#350035")
      (main joker-theme-main-color))
  (custom-theme-set-faces
   `joker
   ;; We don't specify default foreground/background in TTY.
   `(default                        ((((type tty)))
                                     (((type graphic))
                                      :background ,bg
                                      :foreground ,fg)))
   ;; Basics

   `(cursor                         ((t (:background ,white))))
   `(region                         ((t (:background ,region))))
   `(hl-line                        ((((type graphic)) :background ,bg+1)
                                     (((type tty)))))
   `(fringe                         ((t (:background ,bg))))
   `(show-paren-match               ((t (:underline ,green))))
   `(highlight                      ((t (:background ,bg+2))))
   `(button                         ((t (:foreground "#2299CC" :underline t))))
   `(vertical-border                ((t ())))
   `(window-divider                 ((t (:foreground ,bg+3))))
   `(window-divider-first-pixel     ((t (:foreground ,bg+1))))
   `(window-divider-last-pixel      ((t (:foreground ,bg+1))))
   `(line-number                    ((t (:foreground ,bg+3 :inherit default))))
   `(line-number-current-line       ((((type tty)) :foreground ,yellow)
                                     (((type graphic)) :inherit default :foreground ,yellow :background ,bg+1)))
   `(parenthesis                    ((t (:foreground ,fg-1))))
   `(completions-common-part        ((t ())))
   `(minibuffer-prompt              ((t ())))
   `(lazy-highlight                 ((t (:background ,bg+3))))
   `(compilation-info               ((t (:inherit font-lock-function-name-face))))
   `(compilation-warning            ((t (:inherit font-lock-warning-face))))
   `(warning                        ((t (:inherit font-lock-warning-face))))
   `(match                          ((t (:background ,bg+2))))
   `(secondary-selection            ((t (:background ,region2 :extend t))))

   ;; ISearch
   `(isearch                        ((t (:background ,green :foreground "black"))))
   `(isearch-fail                   ((t (:backgronud ,red :foreground "black"))))

   ;; Font Locks
   `(font-lock-comment-face         ((t (:foreground ,blue :italic ,italic))))
   `(font-lock-comment-delimiter-face         ((t (:foreground ,blue :italic ,italic))))
   `(font-lock-string-face          ((t (:foreground ,red))))
   `(font-lock-doc-face             ((t (:foreground ,blue :italic ,italic))))
   `(font-lock-builtin-face         ((t ())))
   `(font-lock-type-face            ((t ())))
   `(font-lock-variable-name-face   ((t ())))
   `(font-lock-keyword-face         ((t (:foreground ,yellow))))
   `(font-lock-constant-face        ((t (:foreground ,purple))))
   `(font-lock-function-name-face   ((t (:bold t))))
   `(font-lock-warning-face         ((t (:foreground ,orange))))
   `(font-lock-preprocessor-face    ((t (:inherit font-lock-constant-face))))

   ;; IMenu
   `(imenu-list-entry-face-0          ((t ())))
   `(imenu-list-entry-subalist-face-0 ((t (:bold t))))

   ;; Mode Line
   `(mode-line                      ((t (:background ,bg-1))))
   `(mode-line-inactive             ((t (:background ,bg+1))))


   ;; Yascroll
   `(yascroll:thumb-fringe          ((t (:background ,main :foreground ,main))))
   `(yascroll:thumb-text-area       ((t (:background ,main :foreground ,main))))

   ;; Company
   `(company-tooltip-common         ((t (:bold t))))
   `(company-tooltip-common-selection ((t (:bold t))))
   `(company-tooltip                ((t (:background ,bg+2))))
   `(company-tooltip-selection      ((t (:background ,bg+3))))
   `(company-tooltip-annotation     ((t (:foreground ,blue))))
   `(company-scrollbar-bg           ((t (:background ,bg+2 :height 0.3))))
   `(company-scrollbar-fg           ((t (:background ,bg+4 :height 0.3))))
   `(company-template-field         ((t (:inherit yas-field-highlight-face))))

   ;; Yasnippet
   `(yas-field-highlight-face       ((t (:background ,region2))))

   ;; Meow
   `(meow-keypad-indicator          ((t (:foreground "black" :background ,red))))
   `(meow-insert-indicator          ((t (:foreground "black" :background ,green))))
   `(meow-normal-indicator          ((t (:foreground "black" :background ,yellow))))
   `(meow-motion-indicator          ((t (:foreground "black" :background ,blue))))
   `(meow-keypad-cursor             ((t ())))
   `(meow-insert-cursor             ((t ())))
   `(meow-normal-cursor             ((t ())))
   `(meow-motion-cursor             ((t ())))

   ;; Cider
   ;;
   `(cider-result-overlay-face      ((t (:background "black"))))
   `(cider-repl-stderr-face         ((t (:foreground ,blue))))
   `(cider-repl-stdout-face         ((t (:foreground ,fg-1))))

   ;; Clojure
   ;;
   `(clojure-character-face         ((t (:foreground ,purple))))

   ;; Ivy
   `(ivy-highlight-face             ((t ())))
   `(ivy-yanked-word                ((t (:background "yellow" :foreground "black"))))
   `(ivy-remote                     ((t ())))
   `(ivy-current-match              ((t (:foreground ,bg :background ,main))))
   `(ivy-minibuffer-match-highlight ((t ())))
   `(ivy-minibuffer-match-face-1    ((t ())))
   `(ivy-minibuffer-match-face-2    ((t ())))
   `(ivy-minibuffer-match-face-3    ((t ())))
   `(ivy-minibuffer-match-face-4    ((t ())))
   `(counsel-outline-default        ((t ())))
   `(swiper-background-match-face-1 ((t (:inherit hl-line))))
   `(swiper-background-match-face-2 ((t (:inherit hl-line))))
   `(swiper-background-match-face-3 ((t (:inherit hl-line))))
   `(swiper-background-match-face-4 ((t (:inherit hl-line))))
   `(swiper-match-face-1            ((t (:foreground "white"))))
   `(swiper-match-face-2            ((t (:foreground "white"))))
   `(swiper-match-face-3            ((t (:foreground "white"))))
   `(swiper-match-face-4            ((t (:foreground "white"))))

   ;; Selectrum
   `(selectrum-current-candidate    ((t (:foreground ,main :inverse-video t))))

   ;; Magit
   `(magit-diff-file-heading-highlight ((t (:background ,bg+1))))
   `(magit-section-highlight           ((t (:background ,bg+1))))
   `(magit-diff-removed             ((t (:inherit font-lock-string-face))))
   `(magit-diff-added               ((t (:inherit font-lock-comment-face))))
   `(magit-diff-removed-highlight   ((t (:inherit font-lock-string-face :background ,bg+2))))
   `(magit-diff-added-highlight     ((t (:inherit font-lock-comment-face :background ,bg+2))))
   `(magit-diff-highlight           ((t (:background ,bg+1))))
   `(magit-diff-context-highlight   ((t (:background ,bg+1))))

   ;; SMerge
   `(smerge-refined-added           ((t (:background "#253325"))))
   `(smerge-lower                   ((t (:background "#173017"))))

   ;; Diff-hl
   `(diff-hl-insert                 ((t (:foreground ,green :background ,green))))
   `(diff-hl-change                 ((t (:foreground ,blue :background ,blue))))
   `(diff-hl-delete                 ((t (:foreground ,red :background ,red))))

   ;; Term
   `(term-color-blue                ((t (:foreground ,blue :background ,blue))))
   `(term-color-green               ((t (:foreground ,green :background ,green))))
   `(term-color-red                 ((t (:foreground ,red :background ,red))))

   ;; Popup
   `(popup-tip-face                 ((t (:background ,bg+4 :foreground ,fg))))
   `(popup-isearch-match            ((t (:background "#CFA300" :foreground "black"))))

   `(tooltip                        ((t ())))
   `(dired-directory                ((t (:foreground ,light-purple))))
   `(web-mode-html-attr-name-face   ((t ())))
   `(web-mode-html-tag-face         ((t ())))

   ;; Emacs Rime
   `(rime-preedit-face              ((t (:underline ,blue :background ,bg+2))))
   `(rime-cursor-face               ((t (:inherit font-lock-constant-face))))
   `(rime-indicator-face            ((t (:foreground ,purple))))
   `(rime-indicator-dim-face        ((t (:foreground ,bg+4))))

   ;; Web Mode
   `(web-mode-function-call-face    ((t ())))
   `(web-mode-function-name-face    ((t ())))
   `(web-mode-html-tag-bracket-face ((t (:inherit parenthesis))))
   `(web-mode-symbol-face           ((t (:foreground ,purple))))
   `(css-selector                   ((t (:foreground ,purple))))

   ;; Markdown
   `(markdown-header-face-1         ((t (:bold t :height ,(nth 0 joker-theme-header-scales)))))
   `(markdown-header-face-2         ((t (:bold t :height ,(nth 1 joker-theme-header-scales)))))
   `(markdown-header-face-3         ((t (:bold t :height ,(nth 2 joker-theme-header-scales)))))
   `(markdown-header-face-4         ((t (:bold t :height ,(nth 3 joker-theme-header-scales)))))
   `(markdown-header-face-5         ((t (:bold t :height ,(nth 4 joker-theme-header-scales)))))
   `(markdown-header-face-6         ((t (:bold t :height ,(nth 5 joker-theme-header-scales)))))
   `(markdown-header-face-7         ((t (:bold t :height ,(nth 6 joker-theme-header-scales)))))

   ;; Telega
   `(telega-entity-type-code        ((t (:inherit font-lock-string-face))))
   `(telega-msg-heading             ((t (:inherit hl-line))))
   `(telega-unmuted-count           ((t (:inherit font-lock-function-name-face))))

   ;; Org-mode
   `(org-todo                      ((t (:foreground ,yellow))))
   `(org-done                      ((t (:foreground ,blue))))
   `(org-headline-todo             ((t (:foreground ,fg+1))))
   `(org-headline-done             ((t (:foreground ,fg-1 :strike-through t))))
   `(org-table                      ((t (:foreground ,fg+1))))
   `(org-level-1                    ((t (:bold t :height ,(nth 0 joker-theme-header-scales)))))
   `(org-level-2                    ((t (:bold t :height ,(nth 1 joker-theme-header-scales)))))
   `(org-level-3                    ((t (:bold t :height ,(nth 2 joker-theme-header-scales)))))
   `(org-level-4                    ((t (:bold t :height ,(nth 3 joker-theme-header-scales)))))
   `(org-level-5                    ((t (:bold t :height ,(nth 4 joker-theme-header-scales)))))
   `(org-level-6                    ((t (:bold t :height ,(nth 5 joker-theme-header-scales)))))
   `(org-level-7                    ((t (:bold t :height ,(nth 6 joker-theme-header-scales)))))
   `(org-document-title             ((t (:inherit font-lock-string-face))))
   `(org-code                       ((t (:inherit font-lock-constant-face))))

   ;; Treemacs
   `(treemacs-root-face             ((t (:inherit font-lock-function-name-face :height 1.4 :underline t))))

   `(fill-column-indicator          ((t (:foreground ,bg+2))))

   `(scroll-bar                     ((t (:foreground ,fg-1))))))

(and load-file-name
     (boundp 'custom-theme-load-path)
     (add-to-list 'custom-theme-load-path
                  (file-name-as-directory
                   (file-name-directory load-file-name))))

(provide-theme 'joker)

;;; joker-theme.el ends here
