;;; -*- lexical-binding: t; -*-

(with-eval-after-load "dired"
  (define-key dired-mode-map "w" #'wdired-change-to-wdired-mode))

(provide 'init-dired)
