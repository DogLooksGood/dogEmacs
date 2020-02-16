(defun m4d--update-cursor-shape ()
  (cond
   (god-local-mode
    (setq cursor-type 'hollow)
    (send-string-to-terminal "\033[3 q"))
   (m4d-insert-mode
    (setq cursor-type '(bar . 5))
    (send-string-to-terminal "\033[5 q"))
   (m4d-normal-mode
    (setq cursor-type 'box)
    (send-string-to-terminal "\033[2 q"))
   (m4d-motion-mode
    (setq cursor-type 'box)
    (send-string-to-terminal "\033[2 q"))))

(defun m4d--switch-modal (modal)
  (cond
   ((equal modal 'normal)
    (m4d-normal-mode 1)
    (m4d-motion-mode -1)
    (m4d-insert-mode -1))
   ((equal modal 'insert)
    (m4d-normal-mode -1)
    (m4d-motion-mode -1)
    (m4d-insert-mode 1))
   ((equal modal 'motion)
    (m4d-normal-mode -1)
    (m4d-motion-mode 1)
    (m4d-insert-mode -1)))
  (m4d--update-cursor-shape))

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
  (and (not (minibufferp))
       (or (member major-mode m4d-motion-mode-list)
           (derived-mode-p 'special-mode))))

(defun m4d--should-enable-normal-p ()
  (and (not (minibufferp))
       (or (equal major-mode 'fundamental-mode)
           view-mode
           (member major-mode m4d-normal-mode-list)
           (derived-mode-p 'text-mode 'conf-mode 'prog-mode))))

(defun m4d--post-command-hook-function ()
  (unless (member major-mode m4d--stick-modes)
    (cond
     ((and (or m4d-insert-mode m4d-normal-mode)
           (m4d--should-enable-motion-p))
      (m4d--switch-modal 'motion)
      (message "Auto switch to MOTION mode."))
     ((and m4d-motion-mode
           (m4d--should-enable-normal-p))
      (m4d--switch-modal 'normal)
      (message "Auto switch to NORMAL mode."))))
  (m4d--update-cursor-shape))

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

(defun m4d--to-normal ()
  (m4d--switch-modal 'normal))

(defun m4d--to-motion ()
  (m4d--switch-modal 'motion))

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
