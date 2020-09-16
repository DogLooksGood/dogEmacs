;;; inf-iex.el --- Run IEx in Emacs                  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  tianshu

;; Author: tianshu <tianshu@tianshu-manjaro>
;; Keywords: languages, tools

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:


(defvar inf-iex-buffer nil
  "IEx session buffer.")

(defvar inf-iex-minor-mode-map
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap (kbd "C-c C-r") 'inf-iex-eval)
    (define-key keymap (kbd "C-c C-k") 'inf-iex-reload)
    (define-key keymap (kbd "C-c C-c C-p") 'inf-iex-compile)
    (define-key keymap (kbd "C-c C-z") 'inf-iex-start)
    keymap))

(define-minor-mode inf-iex-minor-mode
  "Minor mode for Interaction with IEx."
  nil
  "inf-IEx"
  inf-iex-minor-mode-map)

(define-derived-mode inf-iex-mode comint-mode "inf-IEx"
  "Major mode for IEx session buffer.")

(defun inf-iex-eval ()
  (interactive)
  (comint-send-string (get-process "IEx")
                      (->> (buffer-substring-no-properties (region-beginning) (region-end))
                            (replace-regexp-in-string "\n" "\\\\\n")
                           (format "%s\n")
                           (string-trim-left))))

(defun inf-iex-compile ()
  (interactive)
  (when (buffer-modified-p) (save-buffer))
  (comint-send-string (get-process "IEx") (message "c \"%s\"\n" (+smart-file-name))))

(defun inf-iex-reload ()
  (interactive)
  (when (buffer-modified-p) (save-buffer))

  (let ((module-name (save-mark-and-excursion
                       (goto-char (point-min))
                       (re-search-forward
                        "defmodule \\([[:graph:]]+\\)")
                       (match-string 1))))
    (if module-name
        (comint-send-string (get-process "IEx") (message "r %s\n" module-name))
      (message "Can't get module name in this file!"))))

(defun inf-iex--comint-output-filter (arg)
  (message "%s" arg))

(defun inf-iex-start ()
  (interactive)
  (if (and inf-iex-buffer (comint-check-proc inf-iex-buffer))
      (pop-to-buffer inf-elixir-buffer)
    (let* ((name "IEx")
           (cmd (split-string (read-from-minibuffer "Command to start IEx session: " "iex -S mix")))
           (exe (car cmd))
           (args (cdr cmd))
           (comint-buffer (apply #'make-comint-in-buffer name (generate-new-buffer name) exe nil args)))
      (set-buffer comint-buffer)
      (inf-iex-mode)
      (setq inf-iex-buffer (current-buffer))
      ;; (add-hook 'comint-output-filter-functions 'inf-iex--comint-output-filter)
      (pop-to-buffer (current-buffer)))))

(require 'shell)
(require 'term)

(provide 'inf-iex)
;;; inf-iex.el ends here
