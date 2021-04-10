;; Restore the load path
(setq load-path +pdumped-load-path)

;; Disable error message
(setq warning-minimum-level :emergency)

;; These two modes are disabled in pdump
(global-font-lock-mode t)
(transient-mark-mode t)
