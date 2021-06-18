(add-to-list 'load-path (expand-file-name "elisp" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "themes" user-emacs-directory))

(require 'init-straight)
(require 'init-packages)
;; (require 'init-gc)

;; Load private config files if exist.
;; You can override variables in file.
;; An example ~/.emacs.d/private.el :
;;
;; (straight-use-package 'atom-one-dark-theme)
;;
;; (setq +font-family "Fira Code"
;;       +ufont-family "WenQuanYi Micro Hei Mono"
;;       +variable-pitch-family "Inter"
;;       +fixed-pitch-family "Sarasa Mono SC"
;;       +font-size 9
;;       +theme-list '(atom-one-dark joker))

(let ((private-conf (expand-file-name "private.el" user-emacs-directory)))
  (when (file-exists-p private-conf)
    (load-file private-conf)))

(require 'init-defaults)
(require 'init-laf)
(require 'init-font)
