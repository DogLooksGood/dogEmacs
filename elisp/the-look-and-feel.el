;;; Look & Feels

;; Font Setup

(progn
  (setq my-font "IBM Plex Mono-13")
  (set-default-font my-font)
  (add-to-list 'default-frame-alist `(font . ,my-font))
  (set-face-attribute 'default t :font my-font))

;; For glyph

(set-fontset-font t nil "NotoSansMono Nerd Font" nil 'prepend)

;; For Org-mode

(add-hook 'org-mode-hook
          (lambda () (face-remap-add-relative 'default :family "等距更纱黑体 CL")))

;; Transparency Setup

(set-frame-parameter (selected-frame) 'alpha '(95 . 95))
(add-to-list 'default-frame-alist '(alpha . (95 . 95)))

;; Theme Setup

(custom-set-faces
 '(fringe ((t :background nil))))

(use-package nimbus-theme
  :init
  (load-theme 'nimbus t))

;; (use-package zenburn-theme
;;   :init
;;   (load-theme 'zenburn t)
;;   (setq zenburn-scale-org-headlines t)
;;   (custom-theme-set-faces
;;    'zenburn
;;    '(fringe ((t :background nil)))))

(provide 'the-look-and-feel)
