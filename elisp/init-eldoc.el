;;; init-eldoc.el --- Tweaking for eldoc             -*- lexical-binding: t; -*-

;;; Only show eldoc with keybinding.

(bind-key "M-h" 'eldoc global-map)

;;; Don't popup eldoc automatically
(setq eldoc-idle-delay 0.5)

(provide 'init-eldoc)
;;; init-eldoc.el ends here
