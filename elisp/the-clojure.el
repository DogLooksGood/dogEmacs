(use-package clojure-mode
  :bind
  (:map clojure-mode-map
	("C-c C-=" . 'clojure-align))
  :config
  (add-to-list 'clojure-font-lock-keywords
             `(,(concat "\\(:\\{1,2\\}\\)\\(" clojure--sym-regexp "?\\)\\(/\\)\\(" clojure--sym-regexp "\\)")
               (0 'clojure-keyword-face)))
  :init
  (setq clojure-indent-style 'always-indent))

(use-package clj-refactor
  :init
  (setq cljr-warn-on-eval t)
  (setq cljr-suppress-middleware-warnings t))

(use-package cider
  :bind
  (:map cider-mode-map
	("C-." . 'cider-find-var))
  :config
  (unbind-key "M-." cider-mode-map)
  :init
  (setq cider-font-lock-dynamically nil
	cider-font-lock-reader-conditionals nil
	cider-prompt-for-symbol nil))

(defun user/clojure-repl-type ()
  (cond
   ((equal major-mode 'clojure-mode) 'clj)
   ((equal major-mode 'clojurescript-mode) 'cljs)))

;; (defun cider-current-repl (&optional _  _)
;;   (let ((repl-type (user/clojure-repl-type))
;;      (buf-lst (buffer-list)))
;;     (cond
;;      ((equal 'clj repl-type)
;;       (--first (string-match-p "\*cider-repl.+(clj)\*" (buffer-name it))
;;             buf-lst))

;;      ((equal 'cljs cljs)
;;       (--first (string-match-p "\*cider-repl.+(cljs:.+)\*" (buffer-name it))
;;             buf-lst)))))

(defun sesman-current-session (system &optional cxt-types)
  (let ((sessions (or (sesman--linked-sessions system 'sort cxt-types)
                      (sesman--friendly-sessions system 'sort))))
    (if-let ((repl-type (user/clojure-repl-type)))
        (car (--filter (equal repl-type (cider-repl-type (cadr it))) sessions))
      (car sessions))))

(provide 'the-clojure)
