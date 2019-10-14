(use-package clojure-mode
  :bind
  (:map clojure-mode-map
	("C-c C-=" . 'clojure-align))
  :init
  (setq clojure-toplevel-inside-comment-form t)
  :config
  (add-to-list 'clojure-font-lock-keywords
               `(,(concat "\\(:\\{1,2\\}\\)\\("
                          clojure--sym-regexp
                          "?\\)\\(/\\)\\("
                          clojure--sym-regexp
                          "\\)")
               (0 'clojure-keyword-face))))

(use-package clj-refactor
  :bind
  (:map
   clojure-mode-map
   ("/" . 'cljr-slash))
  :init
  (setq cljr-warn-on-eval t)
  (setq cljr-suppress-middleware-warnings t))

(use-package cider
  :bind
  (:map
   cider-mode-map
   ("C-." . 'cider-find-var)
   :map
   cider-repl-mode-map
   ("(" . 'paredit-open-round)
   (")" . 'paredit-close-round)
   ("[" . 'paredit-open-square)
   ("]" . 'paredit-close-square)
   ("<backspace>" . 'paredit-backward-delete))
  :config
  (unbind-key "M-." cider-mode-map)
  :init
  (add-hook 'cider--debug-mode-hook 'user/insert-mode)
  (setq cider-font-lock-dynamically nil
	cider-font-lock-reader-conditionals nil
        cider-repl-use-clojure-font-lock nil
        cider-repl-use-content-types nil
        cider-repl-use-pretty-printing nil
	cider-prompt-for-symbol nil
        cider-enhanced-cljs-completion-p nil))

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
