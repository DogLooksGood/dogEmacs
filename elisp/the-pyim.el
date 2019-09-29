(use-package liberime-config
  :load-path "/home/tianshu/.emacs.d/pyim/liberime.so"
  :config
  (liberime-start (expand-file-name "/usr/share/rime-data")
		  (expand-file-name "~/.emacs.d/pyim/rime/"))
  (liberime-select-schema "xklbdz")
  (setq pyim-default-scheme 'rime))

(use-package pyim
  :config
  ;; 选词框显示5个候选词
  (setq pyim-page-length 5))

(defun my-rime-get (s)
  (when (functionp 'liberime-clear-composition)
    (liberime-clear-composition)
    (dolist (key (string-to-list s))
      (liberime-process-key key))
    (let* ((context (liberime-get-context))
           (menu (alist-get 'menu context))
           (candidates (alist-get 'candidates menu)))
      candidates)))

(push 'pyim-autoselector-xxx pyim-autoselector)

(defun pyim-autoselector-xxx (&rest args)
  (let* ((scheme-name (pyim-scheme-name))
         (class (pyim-scheme-get-option scheme-name :class))
         (prefix (pyim-scheme-get-option scheme-name :code-split-length))
         (words (my-rime-get
                 (concat (pyim-entered-get)
                         (string last-command-event)))))
    (and (pyim-input-chinese-p)
         (not words))))
