;;; -*- lexical-binding: t -*-
(setq comp-deferred-compilation-deny-list ())
(setq straight-vc-git-default-clone-depth 1)

(setq straight-disable-native-compile
      (when (fboundp 'native-comp-available-p)
	(not (native-comp-available-p))))

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(defvar straight-keymap
  (let ((m (make-keymap)))
    (define-key m (kbd "p") 'straight-pull-package)
    (define-key m (kbd "P") 'straight-pull-all)
    (define-key m (kbd "c") 'straight-check-package)
    (define-key m (kbd "C") 'straight-check-all)
    (define-key m (kbd "b") 'straight-rebuild-package)
    (define-key m (kbd "B") 'straight-rebuild-all)
    m))

(defalias 'straight-keymap straight-keymap)
(global-set-key (kbd "C-x M-s") 'straight-keymap)

(provide 'init-straight)
