(defvar +pdump-load-path nil
  "The load-path backup before dump.

This variable is non-nil when emacs is started with dump file.")

(defvar +pdump-packages nil
  "A list of package names to dump.")

(defun +pdump-packages (&rest pkgs)
  "Mark pkgs should be dumped."
  (dolist (pkg pkgs)
    (push pkg +pdump-packages)))

(provide 'init-packages)
