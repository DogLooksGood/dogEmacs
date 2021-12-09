;;; minidark-theme.el --- A minimal dark theme  -*- lexical-binding: t; -*-

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

(defvar minidark-theme-header-scales '(1.6 1.4 1.2 1.0 1.0 1.0 1.0)
  "Scales for headers.")

(let ((bg "#202020")
      (fg "#A0A0A0")
      (cm "#707070")
      (rg "#024347")
      (hl "#2A2A2A")
      (lh "#0DA59B")
      (ss "#353535")
      (kw "#BEBEBE")
      (fn "#bfaa6b")
      (st "#4b9e6b")
      (str "#4a95b5")
      (num "#9466a5")
      (doc "#cc725b")
      (cur "#EFEFEF")
      (pop "#272727"))
  (custom-theme-set-faces
   `minidark
   ;; We don't specify default foreground/background in TTY.
   `(default                        ((((type tty)))
                                     (((type graphic)) :background ,bg :foreground ,fg)))
   ;; Basics
   `(cursor                         ((t (:background ,cur))))
   `(region                         ((t (:background ,rg :extend nil))))
   `(hl-line                        ((t (:background ,hl))))
   `(fringe                         ((t (:background ,bg))))
   `(show-paren-match               ((t (:background ,hl :box (:line-width (-1 . -1) :style nil)))))
   `(highlight                      ((t (:inverse-video t))))
   `(button                         ((t (:box (:line-width (-1 . -1))))))
   `(vertical-border                ((t ())))
   `(window-divider                 ((t (:foreground ,cm))))
   `(window-divider-first-pixel     ((t (:foreground ,cm))))
   `(window-divider-last-pixel      ((t (:foreground ,cm))))
   `(line-number                    ((t (:foreground ,cm))))
   `(line-number-current-line       ((t (:foreground ,fn :background ,hl))))
   `(completions-common-part        ((t ())))
   `(minibuffer-prompt              ((t ())))
   `(lazy-highlight                 ((t (:foreground ,lh :underline t))))
   `(compilation-info               ((t ())))
   `(compilation-warning            ((t ())))
   `(warning                        ((t ())))
   `(match                          ((t (:inverse-video t))))
   `(secondary-selection            ((t (:background ,ss :extend nil))))
   `(help-key-binding               ((t (:bold t))))
   `(shadow                         ((t (:foreground ,cm))))

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
   `(font-lock-keyword-face         ((((type tty)) (:bold t))
                                     (((type graphic)) (:foreground ,kw))))
   `(font-lock-constant-face        ((t (:foreground ,st))))
   `(font-lock-function-name-face   ((t (:foreground ,fn))))
   `(font-lock-warning-face         ((t ())))
   `(font-lock-preprocessor-face    ((t ())))
   `(error                          ((t (:background "red" :foreground ,fn))))

   `(highlight-numbers-number       ((t (:foreground ,num))))

   ;; shell
   `(sh-quoted-exec                 ((t ())))

   ;; IMenu
   `(imenu-list-entry-face-0          ((t ())))
   `(imenu-list-entry-subalist-face-0 ((t (:bold t))))

   ;; Mode Line
   `(tab-line                       ((t ())))
   `(mode-line                      ((t (:background ,fg :foreground ,bg))))
   `(mode-line-inactive             ((t (:background ,cm :foreground ,hl))))
   `(header-line                    ((t ())))
   `(header-line-inactive           ((t ())))

   ;; Company
   `(company-tooltip-common         ((t ())))
   `(company-tooltip-common-selection ((t ())))
   `(company-tooltip                ((t (:background ,pop))))
   `(company-tooltip-search         ((t ())))
   `(company-tooltip-selection      ((t (:inverse-video t))))
   `(company-tooltip-annotation     ((t ())))
   `(company-scrollbar-bg           ((t (:background ,cm))))
   `(company-scrollbar-fg           ((t (:background ,fg))))
   `(company-template-field         ((t (:inherit yas-field-highlight-face))))

   ;; Yasnippet
   `(yas-field-highlight-face       ((t (:background ,hl))))

   ;; Meow
   `(meow-beacon-indicator          ((t (:bold t))))
   `(meow-keypad-indicator          ((t (:bold t))))
   `(meow-insert-indicator          ((t (:bold t))))
   `(meow-normal-indicator          ((t (:bold t))))
   `(meow-motion-indicator          ((t (:bold t))))
   `(meow-keypad-cursor             ((t (:background ,cur))))
   `(meow-insert-cursor             ((t (:background ,cur))))
   `(meow-normal-cursor             ((t (:background ,cur))))
   `(meow-motion-cursor             ((t (:background ,cur))))
   `(meow-unknown-cursor            ((t (:background ,cur))))
   `(meow-beacon-cursor             ((t (:background ,cur))))

   ;; colorful paren
   `(colorful-round           ((t ())))
   `(colorful-square          ((t (:foreground ,str))))
   `(colorful-curly           ((t (:foreground ,st))))
   `(colorful-semicolon       ((t (:foreground ,fn))))

   ;; Cider
   `(cider-result-overlay-face      ((t (:inverse-video t))))
   `(cider-repl-stderr-face         ((t ())))
   `(cider-repl-stdout-face         ((t (:foreground "gray60"))))
   `(cider-test-error-face          ((t (:foreground "yellow" :inverse-video t))))

   ;; Clojure
   `(clojure-character-face         ((t ())))

   ;; Magit
   `(magit-branch-local                ((t (:foreground ,st))))
   `(magit-branch-remote               ((t (:foreground ,fn))))
   `(magit-header-line                 ((t ())))
   `(magit-head                        ((t ())))
   `(magit-section-highlight           ((t (:background ,hl))))
   `(magit-section-heading             ((t ())))
   `(magit-section-selection           ((t ())))
   `(magit-diff-removed                ((t (:background "#311"))))
   `(magit-diff-removed-highlight      ((t (:background "#311"))))
   `(magit-diff-added                  ((t (:background "#131"))))
   `(magit-diff-added-highlight        ((t (:background "#131"))))
   `(magit-diff-context-highlight      ((t (:background ,hl :foreground ,fg))))

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
   `(web-mode-function-call-face    ((t ())))
   `(web-mode-function-name-face    ((t ())))
   `(web-mode-html-tag-bracket-face ((t (:inherit parenthesis))))
   `(web-mode-symbol-face           ((t ())))
   `(css-selector                   ((t ())))
   ;;
   ;; Markdown
   `(markdown-header-face-1         ((t (:underline t :height ,(nth 0 minidark-theme-header-scales)))))
   `(markdown-header-face-2         ((t (:underline t :height ,(nth 1 minidark-theme-header-scales)))))
   `(markdown-header-face-3         ((t (:underline t :height ,(nth 2 minidark-theme-header-scales)))))
   `(markdown-header-face-4         ((t (:underline t :height ,(nth 3 minidark-theme-header-scales)))))
   `(markdown-header-face-5         ((t (:underline t :height ,(nth 4 minidark-theme-header-scales)))))
   `(markdown-header-face-6         ((t (:underline t :height ,(nth 5 minidark-theme-header-scales)))))
   `(markdown-header-face-7         ((t (:underline t :height ,(nth 6 minidark-theme-header-scales)))))
   ;;
   ;; ;; Telega
   `(telega-entity-type-code        ((t ())))
   `(telega-msg-heading             ((t ())))
   `(telega-msg-self-title          ((t (:foreground ,fn))))
   `(telega-unmuted-count           ((t ())))
   ;;
   ;; ;; Org-mode
   `(org-document-title             ((t (:bold t :height ,(nth 0 minidark-theme-header-scales)))))
   `(org-link                       ((t (:underline t))))
   `(org-document-title             ((t ())))
   `(org-code                       ((t (:inherit font-lock-constant-face))))
   `(org-level-1                    ((t (:inherit font-lock-string-face :height ,(nth 0 minidark-theme-header-scales)))))
   `(org-level-2                    ((t (:inherit font-lock-function-name-face :height ,(nth 1 minidark-theme-header-scales)))))
   `(org-level-3                    ((t (:inherit font-lock-keyword-face :height ,(nth 2 minidark-theme-header-scales)))))
   `(org-level-4                    ((t (:height ,(nth 3 minidark-theme-header-scales)))))
   `(org-level-5                    ((t (:height ,(nth 4 minidark-theme-header-scales)))))
   `(org-level-6                    ((t (:height ,(nth 5 minidark-theme-header-scales)))))
   `(org-level-7                    ((t (:height ,(nth 6 minidark-theme-header-scales)))))
   ;;
   ;; ;; Treemacs
   ;; `(treemacs-root-face             ((t (:inherit font-lock-function-name-face :height 1.4 :underline t))))
   `(fill-column-indicator          ((t (:foreground ,cm))))
   `(scroll-bar                     ((t (:foreground ,fg))))
   `(parenthesis                    ((t (:foreground ,cm))))
   `(eldoc-box-body                 ((t (:background ,pop :inherit variable-pitch))))

   `(flycheck-warning               ((t (:underline (:style wave :color ,cm)))))
   `(flycheck-error                 ((t (:underline (:style wave :color ,st)))))
   `(flymake-warning                ((t (:underline (:style wave :color ,st)))))
   `(flymake-error                  ((t (:underline (:style wave :color ,st)))))
   `(flymake-note                   ((t (:underline (:style wave :color ,cm)))))

   `(wgrep-face                     ((t (:underline ,st))))

   `(anzu-mode-line                 ((t (:foreground ,bg :background ,fg))))
   `(erc-nick-default-face          ((t (:inherit font-lock-keyword-face))))
   `(erc-input-face                 ((t (:inherit font-lock-function-name-face))))
   `(erc-timestamp-face             ((t (:inherit font-lock-constant-face))))
   `(erc-notice-face                ((t (:inherit font-lock-comment-face))))

   `(tab-bar                        ((t (:background ,hl))))
   `(tab-bar-tab-group-current      ((t ())))
   `(tab-bar-tab                    ((t (:inverse-video t :bold t))))
   `(tab-bar-tab-group-inactive     ((t ())))
   `(tab-bar-tab-inactive           ((t (:inherit shadow))))

   `(ansi-color-blue                ((t (:foreground "#169edd"))))
   `(ansi-color-bright-blue         ((t (:foreground "#169edd"))))
   `(yascroll:thumb-fringe          ((t (:foreground ,cm :background ,cm))))
   `(yascroll:thumb-text-area       ((t (:foreground ,cm :background ,cm))))

   `(embark-keybinding              ((t (:inherit font-lock-constant-face))))))

(and load-file-name
     (boundp 'custom-theme-load-path)
     (add-to-list 'custom-theme-load-path
                  (file-name-as-directory
                   (file-name-directory load-file-name))))

(provide-theme 'minidark)

;;; minidark-theme.el ends here
