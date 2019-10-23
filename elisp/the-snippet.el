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
  (interactive)
  (yas-abort-snippet)
  (god-local-mode))

(use-package yasnippet
  :bind
  (:map
   yas-keymap
   ("<escape>" . 'user/yas-abort)
   ("<return>" . 'user/yas-next)
   ("M-<return>" . 'newline-and-indent)
   ("S-<return>" . 'yas-prev-field))
  :config
  (yas-reload-all)
  (unbind-key "<tab>" yas-keymap)
  (unbind-key "S-<tab>" yas-keymap)
  :init
  (add-hook 'prog-mode-hook #'yas-minor-mode))

(provide 'the-snippet)
