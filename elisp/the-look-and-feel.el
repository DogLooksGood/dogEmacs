(progn
  (setq my-font "Envy Code R-13")
  (set-default-font my-font)
  (add-to-list 'default-frame-alist `(font . ,my-font))
  (set-face-attribute 'default t :font my-font))

;; (set-fontset-font t nil "NotoSansMono Nerd Font" nil 'prepend)

(set-frame-parameter (selected-frame) 'alpha '(100 . 100))
(add-to-list 'default-frame-alist '(alpha . (100 . 100)))

(use-package zenburn-theme
  :ensure t
  :custom-face
  (fringe ((t :background nil)))
  :init
  (load-theme 'zenburn t))

(provide 'the-look-and-feel)
