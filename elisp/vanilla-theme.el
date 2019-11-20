;;; vanilla-theme.el --- The default emacs theme with some enhancement.

(deftheme vanilla "The default emacs theme with some enhancement.")

(let* ((class '((class color) (min-colors 89)))
       (bg "#fafafa")
       (bg+1 "#e4e4e4")
       (bg+2 "#dedede")
       (fg "#171717")
       (fg-1 "#353535")
       (fg-2 "#505050")
       (hl bg+1)
       (bdr "#999999")
       (sel "#c0d0d0"))
  (custom-theme-set-faces
   'vanilla
   `(default ((t :background ,bg :foreground ,fg)))
   `(hl-line ((t :background ,hl)))
   `(fringe ((t :background nil)))
   `(region ((t :background ,sel :foreground ,fg)))

   ;; diff-hl
   `(diff-hl-insert ((t :foreground "sea green")))
   `(diff-hl-delete ((t :foreground "dark red")))
   `(diff-hl-change ((t :foreground "blue")))

   ;; Line Numbers
   `(line-number ((t :foreground ,fg-2 :background ,bg+1)))
   `(line-number-current-line ((t :inherit line-number :foreground "dark cyan" :bold t)))

   ;; Show Paren
   `(show-paren-match ((t :foreground ,bg :background "dark cyan" :bold t)))
   `(show-paren-match-expression ((t :background ,sel)))

   ;; Font lock
   `(font-lock-keyword-face ((t :foreground "Purple")))

   ;; Eshell
  `(eshell-prompt-face ((t :foreground "Firebrick")))

   ;; Highlight Symbols
   `(highlight-symbol-face ((t :background ,hl)))

   ;; Yasnippet
   `(yas-field-highlight-face ((t :box ,bdr)))

   ;; Leerzeichen
   `(leerzeichen ((t :foreground ,bg)))))

(and load-file-name
     (boundp 'custom-theme-load-path)
     (add-to-list 'custom-theme-load-path
                  (file-name-as-directory
                   (file-name-directory load-file-name))))

(provide-theme 'vanilla)

;;; vanilla-theme.el ends here
