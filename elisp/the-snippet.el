(defun user/yas-sql-mode-fields-to-values (text)
  (string-join
   (mapcar (lambda (s)
             (replace-regexp-in-string
              "\"" ":"
              (replace-regexp-in-string
               "\"$" ""
               (string-trim s))))
           (split-string text ","))
   ", "))

(defun user/yas-next ()
  (interactive)
  (if (company--active-p)
      (company-complete-selection)
    (yas-next-field-or-maybe-expand)))

(defun user/yas-abort ()
  "Abort completion or snippet."
  (interactive)
  (if company-candidates
      (company-abort)
    (progn
      (yas-abort-snippet)
      (when (fboundp 'god-local-mode)
        (god-local-mode)))))

(defun user/yas-start ()
  (god-local-mode -1)
  (setq-local cursor-type '(hbar . 4)))

(use-package yasnippet
  :bind
  (("C-*" . 'yas-insert-snippet)
   :map
   yas-keymap
   ("<escape>" . 'user/yas-abort)
   ("<return>" . 'user/yas-next)
   ("M-<return>" . 'newline-and-indent)
   ("S-<return>" . 'yas-prev-field))
  :config
  (yas-reload-all)
  (add-hook 'yas/before-expand-snippet-hook 'user/yas-start)
  (add-hook 'yas/after-exit-snippet-hook 'user/update-cursor-shape)
  (unbind-key "<tab>" yas-keymap)
  (unbind-key "S-<tab>" yas-keymap)
  :init
  (add-hook 'prog-mode-hook #'yas-minor-mode))

(provide 'the-snippet)
