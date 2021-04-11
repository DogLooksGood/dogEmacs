;; disable native compile.
(setq straight-disable-native-compile t)

;; load the whole configuration
(load (expand-file-name "early-init.el" user-emacs-directory))
(load (expand-file-name "init.el" user-emacs-directory))

;; pdump every packages we marked
(dolist (pkg +pdump-packages)
  (require pkg))

;; backup load-path, restore when startup with dump
(setq +pdump-load-path load-path)

;; dump image
(dump-emacs-portable (expand-file-name "emacs.pdmp" user-emacs-directory))
