;;; -*- lexical-binding: t -*-

;; Setup for TUI Emacs on Linux

(defvar wl-copy-process nil)
(defvar xclip-process nil)

(defun wayland-copy-clipboard (text)
  (setq wl-copy-process (make-process :name "wl-copy"
                                      :buffer nil
                                      :command '("wl-copy" "-f" "-n")
                                      :connection-type 'pipe))
  (process-send-string wl-copy-process text)
  (process-send-eof wl-copy-process))

(defun wayland-copy-primary ()
  (when (use-region-p)
    (setq wl-copy-process (make-process :name "wl-copy"
                                        :buffer nil
                                        :command '("wl-copy" "-p" "-f" "-n")
                                        :connection-type 'pipe))
    (process-send-string wl-copy-process (buffer-substring-no-properties (region-beginning) (region-end)))
    (process-send-eof wl-copy-process)))

(defun wayland-paste ()
  (if (and wl-copy-process (process-live-p wl-copy-process))
      nil ; should return nil if we're the current paste owner
    (shell-command-to-string "wl-paste -n | tr -d \r")))

(defun x11-copy-clipboard (text)
  (setq xclip-process (make-process :name "xclip"
                                    :buffer nil
                                    :command '("xclip" "-sel" "clip")
                                    :connection-type 'pipe))
  (process-send-string xclip-process text)
  (process-send-eof xclip-process))

(defun x11-copy-primary ()
  (when (use-region-p)
    (setq xclip-process (make-process :name "xclip"
                                      :buffer nil
                                      :command '("xclip" "-sel" "primary")
                                      :connection-type 'pipe))
    (process-send-string xclip-process (buffer-substring-no-properties (region-beginning) (region-end)))
    (process-send-eof xclip-process)))

(unless window-system
  ;; enable mouse
  (xterm-mouse-mode 1)

  ;; clipboard setup
  (let ((session (string-trim (shell-command-to-string "echo $XDG_SESSION_TYPE"))))
    (cond
     ((string-equal session "x11")
      (advice-add 'secondary-selection-from-region :after 'x11-copy-primary)
      (setq interprogram-cut-function 'x11-copy-clipboard))
     ((string-equal session "wayland")
      (advice-add 'secondary-selection-from-region :after 'wayland-copy-primary)
      (setq interprogram-cut-function 'wayland-copy-clipboard)
      (setq interprogram-paste-function 'wayland-paste))))

  ;; enable vertical scrolling
  (put 'scroll-left 'disabled nil)
  (put 'scroll-right 'disabled nil)
  (global-set-key (kbd "<mouse-6>")
                  (lambda () (interactive) (scroll-right 1)))
  (global-set-key (kbd "<mouse-7>")
                  (lambda () (interactive) (scroll-left 1))))

(provide 'init-xterm)
