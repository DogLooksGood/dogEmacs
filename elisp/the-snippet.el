(defun user/yas-first (text)
  (car (split-string text " ")))

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
    (yas-abort-snippet)))

(defun user/yas-start ()
  (setq-local cursor-type '(hbar . 4)))

(defun user/yas-init ()
  (yas-reload-all)
  (add-hook 'yas/before-expand-snippet-hook 'user/yas-start)
  (add-hook 'yas/after-exit-snippet-hook 'm4d--update-cursor-shape))

(advice-add 'user/yas-init :around #'user/make-silent)

(defun user/yas-load-local-snippets ()
  (interactive)
  (when (projectile-project-root)
    (let ((local-yas-dir (concat (projectile-project-root) ".yas")))
      (when (file-directory-p local-yas-dir)
        (yas-load-directory local-yas-dir)))))

(make-variable-buffer-local 'yas-snippet-dirs)

(use-package yasnippet
  :bind
  (:map
   yas-keymap
   ("<escape>" . 'user/yas-abort)
   ("C-u" . 'user/yas-abort)
   ("RET" . 'user/yas-next)
   ("<return>" . 'user/yas-next)
   ("M-<return>" . 'newline-and-indent)
   ("S-<return>" . 'yas-prev-field))
  :config
  (user/yas-init)
  (unbind-key "<return>" yas-keymap)
  (unbind-key "S-<return>" yas-keymap)
  (unbind-key "<tab>" yas-keymap)
  (unbind-key "TAB" yas-keymap)
  (unbind-key "S-TAB" yas-keymap)
  :init
  (add-hook 'snippet-mode-hook #'smartparens-mode)
  (add-hook 'prog-mode-hook #'yas-minor-mode)
  (add-hook 'yas-minor-mode-hook #'user/yas-load-local-snippets))

(provide 'the-snippet)
