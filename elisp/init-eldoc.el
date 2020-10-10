;;; init-eldoc.el --- Tweaking for eldoc             -*- lexical-binding: t; -*-

;;; Only show eldoc with keybinding.

(bind-key "M-e" 'eldoc global-map)

;;; Don't popup eldoc automatically
(setq eldoc-idle-delay 9999)

(provide 'init-eldoc)
;;; init-eldoc.el ends here
