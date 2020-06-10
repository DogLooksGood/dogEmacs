;;; -*- lexical-binding: t -*-
;;; Interaction with Tmux

(use-package emamux)

(defun +send-clojure-ns-form ()
  (interactive)
  (let ((ns-form (cider-ns-form)))
    (string-match "^(ns \\([.-a-z]+\\)" ns-form)
    (let ((in-ns-form (format "(doto '%s require in-ns)" (match-string 1 ns-form))))
      (emamux:send-keys in-ns-form))))

(provide 'init-tmux)
