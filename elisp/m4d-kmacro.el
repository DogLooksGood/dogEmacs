(defun m4d--kmacro-post-command-hook ()
  ;; (message "%s" this-command)
  (when god-local-mode
    (unless
        (or (equal this-command 'm4d-god-copy)
            (equal this-command 'm4d-god-exchange))
      (god-local-mode -1))))

(defun m4d--god-advice (fn &rest args)
  (condition-case err
      (progn
        (apply fn args))
    (error (god-local-mode -1)
           (call-interactively #'keyboard-quit))))

(defun m4d--kmacro-mode-setup ()
  (add-hook 'god-local-mode-hook 'm4d--update-cursor-shape)
  (advice-add 'god-mode-lookup-key-sequence :around #'m4d--god-advice)
  (add-hook 'post-command-hook 'm4d--kmacro-post-command-hook))

(provide 'm4d-kmacro)
