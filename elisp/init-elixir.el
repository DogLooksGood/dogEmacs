;;; -*- lexical-binding: t -*-

(require 'inf-iex)

(defface +elixir-dim-face
  '((((class color) (background dark))
     (:foreground "grey60"))
    (((class color) (background light))
     (:foreground "grey40")))
  "Elixir dim face")

(use-package elixir-mode
  :hook (elixir-mode . inf-iex-minor-mode)
  :mode (("\\.eex\\'" . web-mode)
         ("\\.leex\\'" . web-mode))
  :bind
  (:map elixir-mode-map
        ("C-c C-f" . 'elixir-format))
  :config
  (font-lock-add-keywords 'elixir-mode
                          '(("\\([_a-zA-Z0-9!?]+\\):" 1 '+elixir-dim-face)
                            (":[_a-zA-Z0-9\"]+" . font-lock-constant-face)
                            ("defmacro \\([a-zA-Z0-9!?_]+\\)" 1 font-lock-function-name-face)
                            ("@[_a-zA-Z0-9!?]+" . font-lock-constant-face)
                            ("\\<true\\>" . font-lock-constant-face)
                            ("\\<false\\>" . font-lock-constant-face)
                            ("\\<nil\\>" . font-lock-constant-face)
                            ("\\<_\\>" . font-lock-comment-face))))

(use-package mix
  :hook
  ((elixir-mode) . 'mix-minor-mode)
  :custom
  (compilation-scroll-output t))

(defun +elixir-auto-module-name ()
  (let* ((file-name (+smart-file-name))
         (lib-file-name (cond
                         ((string-prefix-p "lib/" file-name)
                          (substring file-name 4))
                         ((string-prefix-p "test/" file-name)
                          (substring file-name 5))
                         (t file-name))))
    (message file-name)
    (-> (replace-regexp-in-string "\.exs?$" "" lib-file-name)
        (split-string "/")
        (->> (-map #'string-inflection-pascal-case-function))
        (string-join "."))))

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

(defun +elixir-compile-tmux ()
  (interactive)
  (when (buffer-modified-p) (save-buffer))
  (emamux:send-keys (message "c \"%s\"" (+smart-file-name))))

(defun +elixir-reload-tmux ()
  (interactive)
  (when (buffer-modified-p) (save-buffer))
  (let ((module-name (save-mark-and-excursion
                       (goto-char (point-min))
                       (re-search-forward
                        "defmodule \\([[:graph:]]+\\)")
                       (match-string 1))))
    (if module-name
        (emamux:send-keys (message "r %s" module-name))
      (message "Can't get module name in this file!"))))

(defun +elixir-post-self-insert-hook-setup ()
  (add-hook 'post-self-insert-hook '+elixir-handle-input nil t))

(add-hook 'elixir-mode-hook '+elixir-post-self-insert-hook-setup)

(use-package polymode
  :mode ("\\.ex\\'" . poly-elixir-mode)
  :config
  (define-hostmode poly-elixir-hostmode :mode 'elixir-mode)
  (define-innermode poly-liveview-expr-elixir-innermode
    :mode 'web-mode
    :head-matcher "^[[:space:]]*~L[\"']\\{3\\}$"
    :tail-matcher "^[[:space:]]*[\"']\\{3\\}$"
    :head-mode 'host
    :tail-mode 'host
    :allow-nested nil
    :keep-in-mode 'host
    :fallback-mode 'host)
  (define-polymode poly-elixir-mode
    :hostmode 'poly-elixir-hostmode
    :innermodes '(poly-liveview-expr-elixir-innermode)))

(provide 'init-elixir)
