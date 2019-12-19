(defun user/update-cursor-shape ()
  (cond
   (god-local-mode
    (setq cursor-type 'box))
   (buffer-read-only
    (setq cursor-type 'hollow))
   (t
    (setq cursor-type '(bar . 5)))))

(defun user/ensure-insert-mode ()
  (when god-local-mode
    (god-local-mode -1)))

(defun user/yank-on-region ()
  (interactive)
  (delete-active-region)
  (call-interactively 'yank)
  (deactivate-mark t))

(use-package selected
  :ensure t
  :bind
  (:map selected-keymap
        ("<escape>" . 'keyboard-escape-quit)
        ("<backspace>" . 'delete-region)
        ("C-y" . 'user/yank-on-region))
  :init
  (selected-global-mode 1))

(defun user/make-silent (func &rest args)
  (cl-letf (((symbol-function 'message)
             (lambda (&rest args) nil)))
    (apply func args)))

(defun user/insert-after ()
  (interactive)
  (forward-char)
  (god-local-mode -1))

(defun user/insert-mode ()
  (interactive)
  (when god-local-mode
    (god-local-mode -1)))

(defun user/seek-sexp ()
  (interactive)
  (unless (equal (point) (point-max))
    (forward-char)
    (while (not (or (looking-at "\\s(")
                    (equal (point) (point-max))))
      (forward-char))))

(defun user/replace-point (ch)
  (interactive "cREPLACE WITH:")
  (delete-char 1)
  (insert-char ch)
  (backward-char))

(defun user/active-region-or-kill-region ()
  (interactive)
  (if (region-active-p)
      (call-interactively #'kill-region)
    (call-interactively #'set-mark-command)))

(defun user/deactivate-region-or-other-buffer ()
  (interactive)
  (if (region-active-p)
      (call-interactively #'keyboard-escape-quit)
    (mode-line-other-buffer)))

(use-package god-mode
  :ensure t
  :quelpa (god-mode
           :fetcher github
           :repo "DogLooksGood/god-mode")
  :bind
  (("M-g" . 'goto-line)
   ("M-k" . 'kill-buffer-and-window)
   ("C-$" . 'shell-command)
   ("C-;" . 'comment-dwim)
   ("C-=" . 'align-regexp)
   ("C-." . 'xref-find-definitions)
   ("C-!" . 'eval-expression)
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
   ("<escape>" . 'user/deactivate-region-or-other-buffer)
   ("<tab>" . 'user/normal-tab)
   ("i" . 'user/insert-mode)
   ("u" . 'undo)
   ("j" . 'join-line)
   ("z" . 'universal-argument)
   ("w" . 'user/active-region-or-kill-region)
   ("*" . 'point-to-register)
   ("@" . 'register-to-point)
   ;; navigation
   ("r" . 'up-list)
   ("f" . 'forward-sexp)
   ("b" . 'backward-sexp)
   ("h" . 'left-char)
   ("t" . 'right-char)
   ("}" . 'scroll-up-command)
   ("{" . 'scroll-down-command)
   ("[" . 'beginning-of-buffer)
   ("]" . 'end-of-buffer))
  ("s" . 'user/seek-sexp)
  :init
  (advice-add 'god-local-mode :around #'user/make-silent)
  (add-hook 'text-mode-hook 'god-local-mode)
  (add-hook 'prog-mode-hook 'god-local-mode)
  (add-hook 'conf-mode-hook 'god-local-mode)
  (add-hook 'god-mode-enabled-hook 'user/update-cursor-shape)
  (add-hook 'god-mode-disabled-hook 'user/update-cursor-shape)
  (setq god-mode-can-omit-literal-key t)
  (setq god-mod-alist
	'((nil . "C-")
	  ("m" . "M-")
      ("SPC" . "C-M-")))
  (setq god-literal-key "SPC"))

(unbind-key "C-x C-g")

(provide 'the-modal)
