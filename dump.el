;;; Load the whole configuration
(load (expand-file-name "init.el" user-emacs-directory))

;;; Some packages to exclude during dump.
(setq +dump-exclude-packages '(oauth2))

;;; Ensure every installed package is loaded.
(dolist (package package-activated-list)
  (unless (member package package-activated-list)
    (require package)))

;;; We have to unload tramp in pdump, otherwise tramp will not work.
(tramp-unload-tramp)

;;; We use this variable to test if we are starting with dump.
(setq +dumped-load-path load-path)

;;; Disable GC
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

;; dump image
(dump-emacs-portable "~/.emacs.d/emacs.pdmp")
