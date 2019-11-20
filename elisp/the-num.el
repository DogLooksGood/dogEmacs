;;; Provide a way to continuously insert numbers without hold shift in programmer dvorak layout.

(use-package hydra
  :init)

(defun dvp-number-0 () (interactive) (insert "0"))
(defun dvp-number-1 () (interactive) (insert "1"))
(defun dvp-number-2 () (interactive) (insert "2"))
(defun dvp-number-3 () (interactive) (insert "3"))
(defun dvp-number-4 () (interactive) (insert "4"))
(defun dvp-number-5 () (interactive) (insert "5"))
(defun dvp-number-6 () (interactive) (insert "6"))
(defun dvp-number-7 () (interactive) (insert "7"))
(defun dvp-number-8 () (interactive) (insert "8"))
(defun dvp-number-9 () (interactive) (insert "9"))
(defun dvp-number-slash () (interactive) (insert "/"))
(defun dvp-number-colon () (interactive) (insert ":"))
(defun dvp-number-minus () (interactive) (insert "-"))
(defun dvp-number-close-curly () (interactive) (insert "}"))

(defhydra hydra-dvp-number (global-map "}")
  "DVP numbers"
  ("[" dvp-number-7 "7")
  ("{" dvp-number-5 "5")
  ("}" dvp-number-3 "3")
  ("(" dvp-number-1 "1")
  ("=" dvp-number-9 "9")
  ("*" dvp-number-0 "0")
  (")" dvp-number-2 "2")
  ("+" dvp-number-4 "4")
  ("]" dvp-number-6 "6")
  ("!" dvp-number-8 "8")
  ("/" dvp-number-slash "/")
  ("-" dvp-number-minus "-")
  (";" dvp-number-colon ":")
  ("<backspace>" backward-delete-char "<="))

;;; This is a hack to unbind paredit-close-curly in clojure-mode.

(defun user/unbind-close-curly-advice (&rest _args)
  (unbind-key "}" clojure-mode-map))

(advice-add 'clojure-paredit-setup :after #'user/unbind-close-curly-advice)

(provide 'the-num)
