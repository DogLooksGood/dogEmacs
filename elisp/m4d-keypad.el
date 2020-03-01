(defvar m4d--keypad-meta-prefix "m")
(defvar m4d--keypad-literal-prefix " ")

(defvar m4d--use-literal nil)
(defvar m4d--use-meta nil)

(defun m4d--keypad-format-key-1 (k)
  (cl-case (car k)
    ('meta (format "M-%s" (cdr k)))
    ('control (format "C-%s" (cdr k)))
    ('literal (cdr k))))

(defun m4d--keypad-format-keys ()
  (let ((result ""))
    (setq result
          (thread-first
              (mapcar #'m4d--keypad-format-key-1 m4d--keypad-keys)
            (reverse)
            (string-join " ")))
    (when m4d--use-meta
      (setq result (concat result " M-")))
    (when m4d--use-literal
      (setq result (concat result " ○")))
    result))

(defun m4d--keypad-quit ()
  (setq m4d--keypad-keys nil
        m4d--use-literal nil
        m4d--use-meta nil)
  (m4d-keypad-mode -1))

(defun m4d--keypad-try-execute ()
  (unless (or m4d--use-literal
              m4d--use-meta)
    (let* ((key-str (m4d--keypad-format-keys))
           (cmd (key-binding (read-kbd-macro key-str))))
      (cond
       ((commandp cmd t)
        (m4d--keypad-quit)
        (call-interactively cmd))
       ((keymapp cmd))
       ((equal 'control (caar m4d--keypad-keys))
        (setcar m4d--keypad-keys (cons 'literal (cdar m4d--keypad-keys)))
        (m4d--keypad-try-execute))
       (t
        (m4d--keypad-quit))))))

(defun m4d-keypad-undo ()
  (interactive)
  (cond
   (m4d--use-literal
    (setq m4d--use-literal nil))
   (m4d--use-meta
    (setq m4d--use-meta nil))
   (t
    (pop m4d--keypad-keys)))
  (unless m4d--keypad-keys
    (m4d--keypad-quit)))

(defun m4d-keypad-self-insert ()
  (interactive)
  (when-let ((key (cond
                   ((equal last-input-event 'return) "RET")
                   ((equal last-input-event 'tab) "<tab>")
                   ((characterp last-input-event)
                    (string last-input-event))
                   (t nil))))
    (cond
     (m4d--use-literal
      (push (cons 'literal
                  (if (string-equal " " key)
                      "SPC"
                    key))
            m4d--keypad-keys)
      (setq m4d--use-literal nil))
     (m4d--use-meta
      (push (cons 'meta key) m4d--keypad-keys)
      (setq m4d--use-meta nil))
     ((and (string-equal key m4d--keypad-meta-prefix)
           (not m4d--use-meta))
      (setq m4d--use-meta t))
     ((and (string-equal key m4d--keypad-literal-prefix)
           (not m4d--use-literal))
      (setq m4d--use-literal t))
     (t
      (push (cons 'control key) m4d--keypad-keys)))
    ;; (message (m4d--keypad-format-keys))
    (unless (or m4d--use-literal
                m4d--use-meta)
      (m4d--keypad-try-execute))))

(provide 'm4d-keypad)
