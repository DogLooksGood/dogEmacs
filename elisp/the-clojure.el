(use-package clojure-mode
  :init
  (setq clojure-toplevel-inside-comment-form t)
  :config
  (unbind-key "}" clojure-mode-map)
  (define-clojure-indent
    (re-frame/reg-event-db :defn)
    (re-fraem/reg-event-fx :defn)
    (re-frame/reg-sub :defn)
    (re-frame/test :defn))
  (add-to-list 'clojure-font-lock-keywords
               `(,(concat "\\(:\\{1,2\\}\\)\\("
                          clojure--sym-regexp
                          "?\\)\\(/\\)\\("
                          clojure--sym-regexp
                          "\\)")
                 (0 'clojure-keyword-face))))

(defun user/cljr-setup ()
  (clj-refactor-mode 1)
  (cljr-add-keybindings-with-prefix "C-c C-r")
  (unbind-key "/" clj-refactor-map))

(use-package clj-refactor
  :bind
  (:map
   clojure-mode-map
   ("/" . 'cljr-slash))
  :init
  (add-hook 'clojure-mode-hook 'user/cljr-setup)
  (setq cljr-warn-on-eval t)
  (setq cljr-suppress-middleware-warnings t))

(use-package cider
  :commands (cider-jack-in cider-jack-in-cljs cider-jack-in-clj&cljs)
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

(defun sesman-current-session (system &optional cxt-types)
  (let ((sessions (or (sesman--linked-sessions system 'sort cxt-types)
                      (sesman--friendly-sessions system 'sort))))
    (if-let ((repl-type (user/clojure-repl-type)))
        (car (--filter (equal repl-type (cider-repl-type (cadr it))) sessions))
      (car sessions))))

(provide 'the-clojure)
