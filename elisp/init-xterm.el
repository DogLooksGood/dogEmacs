(defvar wl-copy-process nil)

(defun wl-copy (text)
  (setq wl-copy-process (make-process :name "wl-copy"
                                     :buffer nil
                                     :command '("wl-copy" "-f" "-n")
                                     :connection-type 'pipe))
  (process-send-string wl-copy-process text)
  (process-send-eof wl-copy-process))

(defun wl-copy-primary ()
  (when (use-region-p)
    (setq wl-copy-process (make-process :name "wl-copy"
                                        :buffer nil
                                        :command '("wl-copy" "-p" "-f" "-n")
                                        :connection-type 'pipe))
    (process-send-string wl-copy-process (buffer-substring-no-properties (region-beginning) (region-end)))
    (process-send-eof wl-copy-process)))

(defun wl-paste ()
  (if (and wl-copy-process (process-live-p wl-copy-process))
      nil ; should return nil if we're the current paste owner
    (shell-command-to-string "wl-paste -n | tr -d \r")))

(unless window-system
  (xterm-mouse-mode 1)
  (advice-add 'secondary-selection-from-region :after 'wl-copy-primary)
  (setq interprogram-cut-function 'wl-copy)
  (setq interprogram-paste-function 'wl-paste))

(provide 'init-xterm)
