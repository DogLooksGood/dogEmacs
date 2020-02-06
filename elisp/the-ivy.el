(use-package ivy
  :ensure t
  :bind
  (("M-z" . 'ivy-resume)
   :map ivy-switch-buffer-map
   ("!" . 'ivy-switch-buffer-kill)
   :map ivy-minibuffer-map
   ("<escape>" . 'keyboard-escape-quit)
   ("C-u" . 'keyboard-escape-quit)
   ("{" . 'ivy-previous-line)
   ("}" . 'ivy-next-line)
   ("[" . 'ivy-previous-history-element)
   ("]" . 'ivy-next-history-element)
   ("<mouse-3>". 'ivy-done)
   ;; ("<tab>" . 'ivy-alt-done)
   ;; ("TAB" . 'ivy-alt-done)
   )
  :init
  (setq ivy-use-virtual-buffers nil
	    ivy-use-selectable-prompt t)
  (ivy-mode 1))

(use-package counsel
  :bind
  ("C-x C-f" . 'counsel-find-file)
  ("<C-escape>" . 'counsel-switch-buffer)
  ("C-? C-f" . 'counsel-describe-function)
  ("C-x C-@" . 'counsel-register)
  ("C-x C-h" . 'counsel-imenu)
  ("C-x C-b" . 'counsel-ibuffer)
  ("M-y" . 'counsel-yank-pop))

(use-package counsel-projectile
  :init
  (counsel-projectile-mode 1))

(provide 'the-ivy)
