(defun m4d--save-position-record ()
  (let ((first-pos (car m4d--position-record)))
    (unless (and first-pos
                 (equal (point) (car first-pos))
                 (equal (mark) (cdr first-pos)))
      (let ((pos (cons (point) (mark))))
        (push pos m4d--position-record)))))

(defun m4d--pop-position-record ()
  (pop m4d--position-record)
  (when-let ((pos (car m4d--position-record)))
    (goto-char (car pos))
    (push-mark (cdr pos) t t)))

(defun m4d--should-enable-motion-p ()
  (or (minibufferp)
      (member major-mode m4d-motion-mode-list)
      (derived-mode-p 'special-mode)))

(defun m4d--should-enable ()
  (and (or (equal major-mode 'fundamental-mode)
           view-mode
           (member major-mode m4d-enable-mode-list)
           (derived-mode-p 'text-mode 'conf-mode 'prog-mode))))

(defun m4d--update-cursor-shape ()
  (cond
   (god-local-mode
    (setq cursor-type 'hollow))
   ((and (m4d--should-enable) (not m4d-normal-mode))
    (setq cursor-type '(bar . 5))
    (unless (display-graphic-p)
      (send-string-to-terminal "\e[6 q")))
   (m4d-normal-mode
    (setq cursor-type 'box)
    (unless (display-graphic-p)
      (send-string-to-terminal "\e[2 q")))
   (m4d-motion-mode
    (setq cursor-type 'box)
    (unless (display-graphic-p)
      (send-string-to-terminal "\e[2 q")))))

(defun m4d--direction-right-p ()
  (if (region-active-p)
      (>= (point) (mark))
    t))

(defun m4d--start-select ()
  (push-mark (point) t t))

(defun m4d--clear-select ()
  (setq m4d--position-record nil)
  (setq m4d--last-select nil)
  (when (region-active-p)
    (deactivate-mark)))

(defun m4d--keep-select ()
  (unless (region-active-p)
    (push-mark (point) t t)))

(defun m4d--execute-kbd-macro (keys)
  (when-let ((cmd (key-binding (read-kbd-macro keys))))
    (call-interactively cmd)))

(defun m4d--select-thing (thing &optional direction-right)
  (when-let ((bounds (bounds-of-thing-at-point thing)))
    (let* ((beg (car bounds))
           (end (cdr bounds)))
      (when (and beg end)
        (push-mark (if direction-right beg end) t t)
        (goto-char (if direction-right end beg))))))

(defun m4d--in-string-p ()
  "Return if we are in string."
  (nth 3 (syntax-ppss)))

(defun m4d--get-mode-leader-keymap (mode &optional ensure)
  "Return the leader keymap for mode.
If ensure is t, create new if not found."
  (if-let ((keymap (plist-get m4d--leader-mode-keymaps mode)))
      keymap
    (if ensure
      (let ((keymap (make-sparse-keymap)))
        (set-keymap-parent keymap m4d-leader-base-keymap)
        (setq m4d--leader-mode-keymaps (plist-put m4d--leader-mode-keymaps mode keymap))
        keymap)
      m4d-leader-base-keymap)))


(provide 'm4d-util)
