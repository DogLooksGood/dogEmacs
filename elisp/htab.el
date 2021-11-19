;;; -*- lexical-binding: t -*-

(require 'dash)

(defcustom htab--tabs-num 5
  "The number of tabs."
  :group 'htab
  :type 'number)

(defcustom htab-ignore-commands
  '(other-window
    quit-window
    mouse-set-point
    mouse-drag-region
    minibuffer-keyboard-quit
    minibuffer-quit)
  "Commands to ignored when buffer list changed.")

(defface htab-face
  '((t (:height 0.85)))
  "Face for all htab tabs.")

(defvar htab--window-states nil
  "Recent buffers, buffer indexes for each window.

The structure should be: ((window index buffers) ...)")

(defvar htab--preserve-tabs nil)

(defun htab--filter-buffer (buf)
  (or (eq buf (current-buffer))
      (not (string-prefix-p " " (buffer-name buf)))))

(defun htab--recent-buffers ()
  (let* ((w (selected-window))
         (curr-buf (current-buffer))
         ;; exclude the displayed buffers of other live windows
         (exclude-bufs (-some->> (window-list)
                         (--remove-first (eq w it))
                         (-filter #'window-live-p)
                         (--mapcat (caddr (assoc it htab--window-states)))
                         (--reject (eq curr-buf it)))))
    (->> (buffer-list)
         (-filter #'htab--filter-buffer)
         (--remove (memq it exclude-bufs))
         (-take htab--tabs-num))))

(defun htab--update-window-buffers ()
  (let ((win (selected-window)))
    (let ((bufs (htab--recent-buffers)))
      (setq htab--window-states
            (--filter (window-live-p (car it))
                      htab--window-states))
      (setf (alist-get win htab--window-states)
            (list 0 bufs)))))

(defun htab--on-buffer-list-updated ()
  (when this-command                    ; will only update after a command
    (if htab--preserve-tabs
        (setq htab--preserve-tabs nil)
      (when (or (not (memq this-command htab-ignore-commands))
                (null (assoc (selected-window) htab--window-states)))
        (htab--update-window-buffers)))))

(defun htab--on-buffer-killed ()
  (setq htab--window-states
        (-map (-lambda ((w idx bufs))
                (list w idx (--reject (eq (current-buffer) it) bufs)))
              htab--window-states)))

(defun htab--switch-to-buffer-at-index (w idx)
  (-let* (((_ bufs) (alist-get w htab--window-states)))
    (select-window w)
    (setq htab--preserve-tabs t)
    (switch-to-buffer (nth idx bufs))
    (setf (alist-get w htab--window-states)
          (list idx bufs))))

(defun htab--make-keymap-for-index (w idx)
  (let ((keymap (make-keymap)))
    (define-key keymap [header-line mouse-1] (lambda () (interactive) (htab--switch-to-buffer-at-index w idx)))
    (define-key keymap [mode-line mouse-1] (lambda () (interactive) (htab--switch-to-buffer-at-index w idx)))
    (define-key keymap [tab-line mouse-1] (lambda () (interactive) (htab--switch-to-buffer-at-index w idx)))
    keymap))

(defun htab-indicator ()
  "Indicator used in header-line-format."
  (if-let ((state (assoc (selected-window) htab--window-states)))
      (-let (((w idx bufs) state))
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
                        (propertize (format " %s " s)
                                    'face '(htab-face font-lock-keyword-face))
                      (propertize (format " %s " s)
                                  'face '(htab-face shadow)
                                  'keymap (htab--make-keymap-for-index w it-index))))
                  bufs)
                 (propertize (format "%c" #x10f55) 'face 'shadow))))
    " ... "))

(defun htab-next-buffer ()
  "Switch to buffer at next tab."
  (interactive)
  (when-let ((state (assoc (selected-window) htab--window-states)))
    (-let (((_ idx bufs) state))
      (when bufs
        (if (= idx (1- (length bufs)))
            (let ((buf (car bufs)))
              (switch-to-buffer buf)
              (setf (alist-get (selected-window) htab--window-states)
                    (list 0 bufs)))
          (let ((buf (nth (1+ idx) bufs)))
            (switch-to-buffer buf)
            (setf (alist-get (selected-window) htab--window-states)
                  (list (1+ idx) bufs))))))))

(defun htab-prev-buffer ()
  "Switch to buffer at previous tab."
  (interactive)
  (when-let ((state (assoc (selected-window) htab--window-states)))
    (-let (((_ idx bufs) state))
      (when bufs
        (if (zerop idx)
            (let ((buf (-last-item bufs)))
              (switch-to-buffer buf)
              (setf (alist-get (selected-window) htab--window-states)
                    (list (1- (length bufs)) bufs)))
          (let ((buf (nth (1- idx) bufs)))
            (switch-to-buffer buf)
            (setf (alist-get (selected-window) htab--window-states)
                  (list (1- idx) bufs))))))))

(defun htab-mode--init ()
  (if (not htab-mode)
      (progn
        (remove-hook 'kill-buffer-hook 'htab--on-buffer-killed)
        (remove-hook 'buffer-list-update-hook 'htab--on-buffer-list-updated))
    (add-hook 'buffer-list-update-hook 'htab--on-buffer-list-updated)
    (add-hook 'kill-buffer-hook 'htab--on-buffer-killed)
    (htab--update-window-buffers)))

(define-minor-mode htab-mode
  "Display independent tabs for each buffer."
  :init-value nil
  (htab-mode--init))

(provide 'htab)
;; htab.el ends here
