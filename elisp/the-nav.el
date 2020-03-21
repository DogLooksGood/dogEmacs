;;; packages for NAV

(use-package highlight-symbol
  :hook
  (prog-mode . highlight-symbol-mode)
  :bind
  (("M-n" . 'highlight-symbol-next)
   ("M-p" . 'highlight-symbol-prev))
  :custom
  (highlight-symbol-idle-delay 0.5)
  (highlight-symbol-highlight-single-occurrence nil))

(use-package phi-search
  :bind
  (
   ("C-s" . 'phi-search)
   :map
   phi-search-default-map
   ("<escape>" . 'phi-search-abort)))

(provide 'the-nav)
