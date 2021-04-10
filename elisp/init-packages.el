(defvar +pdump-packages nil
  "A list of package names to dump.")

(defun +pdump-packages (&rest pkgs)
  (dolist (pkg pkgs)
    (push pkg +pdump-packages)))

(provide 'init-packages)
