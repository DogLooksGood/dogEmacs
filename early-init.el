(add-to-list 'load-path (expand-file-name "elisp" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "themes" user-emacs-directory))

(require 'init-straight)
(require 'init-packages)

;;; Personal configuration may override some variables
(let ((private-conf (expand-file-name "private.el" user-emacs-directory)))
  (when (file-exists-p private-conf)
    (load-file private-conf)))

(require 'init-gc)
(require 'init-defaults)
(require 'init-laf)
