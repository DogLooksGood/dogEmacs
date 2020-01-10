(defun user/to-register ()
  (interactive)
  (cond
   ((region-active-p)
    (call-interactively #'copy-to-register)
    (keyboard-quit))
   ((equal last-command 'kmacro-end-macro)
    (call-interactively #'kmacro-to-register))
   (current-prefix-arg
    (call-interactively #'window-configuration-to-register))
   (t
    (call-interactively #'point-to-register))))

(bind-key "C-*" 'user/to-register)
(bind-key "C-&" 'register-to-point)

(provide 'the-register)
