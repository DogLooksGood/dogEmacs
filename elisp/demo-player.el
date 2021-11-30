;;; -*- lexical-binding: t -*-

(defvar demo-player-leading "▶")

(defvar demo-player-command-interval 0.3)

(defvar demo-player-highlight nil)

(defun demo-player-run ()
  (let (res)
    (save-mark-and-excursion
      ;; goto previous position
      (if (not (and (overlayp demo-player-highlight)
                    (overlay-start demo-player-highlight)))
          (goto-char (point-min))
        (goto-char (overlay-end demo-player-highlight))
        (delete-overlay demo-player-highlight))
      ;; search for next leading
      (when (search-forward demo-player-leading nil t)
        (let* ((end (save-mark-and-excursion
                      (if (not (search-forward demo-player-leading (line-end-position) t))
                          (line-end-position)
                        (forward-char (- (length demo-player-leading)))
                        (point))))
               (ov (make-overlay (point) end)))

          (setq res (thread-first
                      (buffer-substring-no-properties (point) end)
                      (string-trim))
                demo-player-highlight ov)
          (overlay-put ov 'face 'highlight))))

    (when res
      ;; executing commands or insert texts
      (if (string-equal "<minibuf>" res)
          (add-hook 'minibuffer-setup-hook 'demo-player-delay-run)
        (if-let ((cmd (key-binding res)))
            (progn
              (setq this-command cmd)
              (call-interactively cmd))
          (insert res)))
      ;; schedule next
      (run-at-time demo-player-command-interval nil #'demo-player-run))))

(defun demo-player-delay-run ()
  (remove-hook 'minibuffer-setup-hook 'demo-player-delay-run)
  (sit-for demo-player-command-interval))

(defun demo-player-cancel ()
  (interactive)
  (when (overlayp demo-player-highlight)
    (delete-overlay demo-player-highlight)))

(defun demo-player-start ()
  (interactive)
  (demo-player-run))

(provide 'demo-player)
