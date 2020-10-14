;;; -*- lexical-binding: t -*-

(defface +elixir-dim-face
  '((((class color) (background dark))
     (:foreground "grey60"))
    (((class color) (background light))
     (:foreground "grey40")))
  "Elixir dim face.")

(use-package elixir-mode
  :mode (("\\.eex\\'" . web-mode)
         ("\\.leex\\'" . web-mode))
  :bind
  (:map elixir-mode-map
        ("C-c C-f" . 'elixir-format))
  :config
  (font-lock-add-keywords 'elixir-mode
                          '(("\\([_a-zA-Z0-9!?]+\\):" 1 'default)
                            (":[_a-zA-Z0-9\"!?]+" . font-lock-constant-face)
                            ("defmacro \\([a-zA-Z0-9!?_]+\\)" 1 font-lock-function-name-face)
                            ("\\_<@[_a-zA-Z0-9!?]+\\_>" . 'default)
                            ("\\_<true\\_>" . font-lock-constant-face)
                            ("\\_<false\\_>" . font-lock-constant-face)
                            ("\\_<nil\\_>" . font-lock-constant-face)
                            ("\\_<_[a-zA-Z0-9]*\\_>" . '+elixir-dim-face))))

;; (defun inf-iex-patch-syntax-table ()
;;   (modify-syntax-entry ?_ "_" elixir-mode-syntax-table)
;;   (modify-syntax-entry ?% "_" elixir-mode-syntax-table)
;;   (modify-syntax-entry ?? "-" elixir-mode-syntax-table))
;;
;; (inf-iex-patch-syntax-table)

(modify-syntax-entry ?& "'" elixir-mode-syntax-table)

(use-package inf-iex
  :quelpa
  (inf-iex :fetcher file :path "~/source/inf-iex")
  :hook (elixir-mode . inf-iex-minor-mode))

(use-package mix
  :hook
  ((elixir-mode) . 'mix-minor-mode)
  :config
  (unbind-key "C-c C-c" mix-minor-mode-map)
  (bind-key "C-c C-t t" 'mix-test-current-test)
  (bind-key "C-c C-t b" 'mix-test-current-buffer)
  (bind-key "C-c C-t p" 'mix-test)
  :custom
  (compilation-scroll-output t))

(defun +elixir-handle-input ()
  (unless (or (+in-string-p) (+in-comment-p))
    (cond
     ((looking-back ",,$" 2)
      (backward-delete-char 2)
      (insert "|> "))
     ((looking-back "<-" 2))
     ((looking-back "[[:graph:]]-" 2)
      (backward-delete-char 1)
      (insert "_"))
     ((looking-back ";" 2)
      (backward-delete-char 1)
      (insert ":")))))

(defun +elixir-post-self-insert-hook-setup ()
  (add-hook 'post-self-insert-hook '+elixir-handle-input nil t))

(add-hook 'elixir-mode-hook '+elixir-post-self-insert-hook-setup)

(provide 'init-elixir)
