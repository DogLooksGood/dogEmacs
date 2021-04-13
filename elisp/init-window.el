;;; -*- lexical-binding: t -*-

(defun +split-window-dwim ()
  "Split window.

If the window is wide enough, split at right, otherwise split at below."
  (interactive)
  (if (> (window-width) 160)
      (split-window-right)
    (split-window-below)))

(defun +rotate-window ()
  "Rotate all windows clockwise."
  (interactive)
  (let* ((wl (window-list nil nil (minibuffer-window)))
         (bl (reverse (mapcar (lambda (w) (window-buffer w)) wl)))
         (nbl (append (cdr bl) (list (car bl)))))
    (cl-loop for w in wl
          for b in (reverse nbl)
          do (set-window-buffer w b))
    (select-window (next-window))))

(provide 'init-window)
