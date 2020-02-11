(defun m4d-leader-define-key (&rest args)
  (mapcar (lambda (key-def)
            (define-key m4d-leader-base-keymap
              (kbd (car key-def))
              (cdr key-def)))
          args))

(defun m4d-leader-define-mode-key (mode &rest args)
  (when-let ((keymap (m4d--get-mode-leader-keymap mode t)))
    (mapcar (lambda (key-def)
              (define-key keymap
                (kbd (car key-def))
                (cdr key-def)))
            args)))

(defun m4d-normal-define-key (&rest args)
  (mapcar (lambda (key-def)
            (define-key m4d-normal-keymap
              (kbd (car key-def))
              (cdr key-def)))
          args))

(provide 'm4d-helpers)
