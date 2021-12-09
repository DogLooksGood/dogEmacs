(setq gc-cons-threshold 50000000)

(add-to-list 'load-path (expand-file-name "elisp" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "themes" user-emacs-directory))

(require 'init-straight)
(require 'init-gc)

;; Load private config files if exist.
;; You can override variables in file.
;; An example ~/.emacs.d/private.el :
;;
;; (straight-use-package 'atom-one-dark-theme)
;;
;; (setq +font-family "Fira Code"
;;       +font-unicode-family "WenQuanYi Micro Hei Mono"
;;       +variable-pitch-family "Inter"
;;       +fixed-pitch-family "Sarasa Mono SC"
;;       +font-size 9
;;       +theme-list '(atom-one-dark joker))

(let ((private-conf (expand-file-name "private.el" user-emacs-directory)))
  (when (file-exists-p private-conf)
    (load-file private-conf)))

(defun measure-require (pkg)
  (message "require: %s" pkg)
  (+measure-time-1
   (require pkg)))

(require 'init-util)
(require 'init-defaults)
(require 'init-modeline)
(require 'init-laf)
(require 'init-tab-bar)
