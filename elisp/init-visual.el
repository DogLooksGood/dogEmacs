(bind-key "C-S-L" 'display-line-numbers-mode)

(setq display-line-numbers-width 4)

(add-hook 'prog-mode-hook 'display-line-numbers-mode)

(global-hl-line-mode 1)

(use-package rainbow-mode
  :hook
  ((css-mode . rainbow-mode)
   (javascript-mode . rainbow-mode)
   (mhtml-mode . rainbow-mode)
   (web-mode . rainbow-mode)))

(use-package hideshow
  :hook
  (prog-mode . hs-minor-mode)
  :bind
  (("C-S-H" . 'hs-toggle-hiding)
   ("<C-tab>" . 'hs-toggle-hiding)))

(use-package highlight-numbers
  :hook
  (prog-mode . highlight-numbers-mode))

(use-package paren-face
  :hook
  (prog-mode . paren-face-mode)
  :custom
  (paren-face-regexp "[][(){}]"))

;; (use-package yascroll
;;   :init
;;   (global-yascroll-bar-mode -1))

;; (use-package olivetti
;;   :hook ((text-mode conf-mode prog-mode) . olivetti-mode)
;;   :custom
;;   (olivetti-body-width 120))

;; (scroll-bar-mode -1)


(provide 'init-visual)
