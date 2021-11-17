;;; -*- lexical-binding: t -*-

(require 'dash)

(defvar mbuf--ignore-commands
  '(other-window
    meow-quit
    quit-window
    mouse-set-point
    mouse-drag-region))

(defvar mbuf--window-states nil
  "Recent buffers, buffer indexes for each window.

The structure should be: ((window index buffers) ...)")

(defun mbuf--recent-buffers ()
  (let* ((w (selected-window))
         (curr-buf (current-buffer))
         ;; exclude the current buffer of each live window
         (exclude-bufs (-some->> (window-list)
                         (--remove-first (eq w it))
                         (-filter #'window-live-p)
                         (--mapcat (caddr (assoc it mbuf--window-states)))
                         (--remove-first (eq curr-buf it)))))
    (->> (buffer-list)
         (-remove #'minibufferp)
         (--remove (memq it exclude-bufs))
         (-take 3))))

(defun mbuf--update-window-buffers ()
  (let ((w (selected-window)))
    (let ((bufs (mbuf--recent-buffers)))
      (setf (alist-get w mbuf--window-states)
            (list 0 bufs)))))

(defun mbuf--on-buffer-list-updated ()
  (when (or (not (memq this-command mbuf--ignore-commands))
            (null (assoc (selected-window) mbuf--window-states)))
    (mbuf--update-window-buffers)))

(add-hook 'buffer-list-update-hook 'mbuf--on-buffer-list-updated)

(defun mbuf-mode-line-section ()
  (if-let ((state (assoc (selected-window) mbuf--window-states)))
      (-let (((_ idx bufs) state))

        (format " %s "
                (string-join
                 (--map-indexed
                  (let ((s (buffer-name it))
                        (keymap (make-keymap)))
                    (define-key keymap
                                [header-line mouse-1]
                                (lambda ()
                                  (interactive)
                                  (switch-to-buffer it)))
                    (if (= idx it-index)
                        (propertize s 'face 'font-lock-keyword-face)
                      s))
                  bufs)
                 " | ")))
    " uninitialized "))

(defun mbuf-next-buffer ()
  (interactive)
  (when-let ((state (assoc (selected-window) mbuf--window-states)))
    (-let (((_ idx bufs) state))
      (when bufs
        (if (= idx (1- (length bufs)))
            (let ((buf (car bufs)))
              (switch-to-buffer buf)
              (setf (alist-get (selected-window) mbuf--window-states)
                    (list 0 bufs)))
          (let ((buf (nth (1+ idx) bufs)))
            (switch-to-buffer buf)
            (setf (alist-get (selected-window) mbuf--window-states)
                  (list (1+ idx) bufs))))))))

(defun mbuf-prev-buffer ()
  (interactive)
  (when-let ((state (assoc (selected-window) mbuf--window-states)))
    (-let (((_ idx bufs) state))
      (when bufs
        (if (zerop idx)
            (let ((buf (-last-item bufs)))
              (switch-to-buffer buf)
              (setf (alist-get (selected-window) mbuf--window-states)
                    (list (1- (length bufs)) bufs)))
          (let ((buf (nth (1- idx) bufs)))
            (switch-to-buffer buf)
            (setf (alist-get (selected-window) mbuf--window-states)
                  (list (1- idx) bufs))))))))

(provide 'mbuf)
;; mbuf.el ends here
