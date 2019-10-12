(defun my-update-cursor ()
  (setq cursor-type (if (or god-local-mode buffer-read-only)
                        'box
                      '(bar . 4))))

(use-package selected
  :ensure t
  :bind
  (:map selected-keymap
	("<backspace>" . 'delete-region))
  :init
  (selected-global-mode 1))

(require 'conf-mode)

(defun user/make-silent (func &rest args)
  (cl-letf (((symbol-function 'message)
             (lambda (&rest args) nil)))
    (apply func args)))

(use-package god-mode
  :ensure t
  :bind
  (("C-$" . end-of-line)
   ("C-x C-k" . 'kill-buffer)
   ("C-}" . 'scroll-up-command)
   ("C-{" . 'scroll-down-command)
   ("<escape>" . mode-line-other-buffer)
   ("C-@" . delete-indentation)
   ("C-r" . 'point-to-register)
   ("C-;" . 'comment-dwim)
   ("C-x C-x" . 'save-buffer)
   ("C-=" . 'indent-region)
   ("C-." . 'xref-find-definitions)
   ("C-," . 'xref-pop-marker-stack)
   :map
   minibuffer-local-map
   ("<escape>" . 'keyboard-escape-quit)
   :map
   text-mode-map
   ("<escape>" . 'god-local-mode)
   :map
   conf-mode-map
   ("<escape>" . 'god-local-mode)
   :map
   prog-mode-map
   ("<escape>" . 'god-local-mode)
   :map
   god-local-mode-map
   ("<escape>" . mode-line-other-buffer)
   ("u" . 'god-local-mode)
   ("e" . 'repeat)
   ("m" . 'back-to-indentation)
   ("h" . 'backward-word)
   ("t" . 'forward-word)
   ("j" . 'jump-to-register)
   ("&" . 'universal-argument)
   ("[" . 'beginning-of-buffer)
   ("]" . 'end-of-buffer)
   ("q" . 'delete-window)
   ("v" . 'kill-ring-save))
  :init
  (advice-add 'god-local-mode :around #'user/make-silent)
  (add-hook 'text-mode-hook 'god-local-mode)
  (add-hook 'prog-mode-hook 'god-local-mode)
  (add-hook 'conf-mode-hook 'god-local-mode)
  (add-hook 'god-mode-enabled-hook 'my-update-cursor)
  (add-hook 'god-mode-disabled-hook 'my-update-cursor)
  (setq god-mod-alist
	'((nil . "C-")
	  ("i" . "M-")
          ("o" . "C-M-")))
  (setq god-literal-key "SPC"))


(provide 'the-modal)
