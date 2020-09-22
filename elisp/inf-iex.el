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

(defvar inf-iex-minor-mode-map
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap (kbd "C-c C-v") 'inf-iex-toggle-send-target)
    (define-key keymap (kbd "C-c C-r") 'inf-iex-eval)
    (define-key keymap (kbd "C-c C-l") 'inf-iex-eval-line)
    (define-key keymap (kbd "C-c C-k") 'inf-iex-reload)
    (define-key keymap (kbd "C-c C-c C-p") 'inf-iex-compile)
    (define-key keymap (kbd "C-c C-z") 'inf-iex-start)
    keymap)
  "Keymap for interaction with IEx buffer.")

(defvar inf-iex-send-target
  'process
  "Can be `process' or `tmux'.")

(defvar inf-iex-mode-map
  (let ((keymap (make-sparse-keymap)))
    ;; (define-key keymap (kbd "TAB") 'completion-at-point)
    keymap)
  "Keymap for IEx buffer.")

(define-minor-mode inf-iex-minor-mode
  "Minor mode for Interaction with IEx."
  nil
  "inf-IEx"
  inf-iex-minor-mode-map)

(define-derived-mode inf-iex-mode comint-mode "inf-IEx"
  "Major mode for IEx session buffer."
  nil
  "IEx"
  inf-iex-mode-map)

(defun inf-iex-toggle-send-target ()
  (interactive)
  (message "Set inf-iex send target to %s"
           (if (eq 'process inf-iex-send-target)
               (setq inf-iex-send-target 'tmux)
             (setq inf-iex-send-target 'process))))

(defun inf-iex--tmux-send (input)
  (interactive)
  (emamux:check-tmux-running)
  (condition-case nil
      (progn
        (if (or current-prefix-arg (not (emamux:set-parameters-p)))
            (emamux:set-parameters))
        (let ((target (emamux:target-session)))
          (setq emamux:last-command input)
          (emamux:reset-prompt target)
          (emamux:send-keys input)))
    (quit (emamux:unset-parameters))))

(defun inf-iex--send (string)
  (cond
   ((eq 'process inf-iex-send-target)
    (comint-send-string (inf-iex--get-process) (format "%s\n" string)))
   ((eq 'tmux inf-iex-send-target)
    (inf-iex--tmux-send string))))

(defun inf-iex-eval ()
  (interactive)
  (inf-iex--send
   (->> (buffer-substring-no-properties (region-beginning) (region-end))
        (replace-regexp-in-string "^#" "")
        (replace-regexp-in-string "\n#" "\n")
        (replace-regexp-in-string "\n" " \\ \n")
        (format "%s")
        (string-trim-left))))

(defun inf-iex-eval-line ()
  (interactive)
  (inf-iex--send
   (->> (buffer-substring-no-properties (line-beginning-position) (line-end-position))
        (string-remove-prefix "# ")
        (format "%s")
        (string-trim-left))))

(defun inf-iex-compile ()
  (interactive)
  (when (buffer-modified-p) (save-buffer))
  (inf-iex--send (message "c \"%s\"\n" (+smart-file-name))))

(defun inf-iex-reload ()
  (interactive)
  (when (buffer-modified-p) (save-buffer))

  (let ((module-name (save-mark-and-excursion
                       (goto-char (point-min))
                       (re-search-forward
                        "defmodule \\([[:graph:]]+\\)")
                       (match-string 1))))
    (if module-name
        (inf-iex--send (message "r %s\n" module-name))
      (message "Can't get module name in this file!"))))

(defun inf-iex--make-iex-buffer-name ()
  (format "IEx[%s]" (project-root (project-current))))

(defun inf-iex--get-process ()
  (get-process (inf-iex--make-iex-buffer-name)))

(defun inf-iex-start ()
  (interactive)
  (let ((inf-iex-buffer (inf-iex--make-iex-buffer-name)))
    (if (and inf-iex-buffer (comint-check-proc inf-iex-buffer))
        (pop-to-buffer inf-iex-buffer)
      (let* ((proj-root (project-root (project-current)))
             (name (inf-iex--make-iex-buffer-name))
             (cmd (split-string (read-from-minibuffer "Command to start IEx session: " "iex -S mix")))
             (exe (car cmd))
             (args (cdr cmd))
             (comint-buffer
              (let ((default-directory proj-root))
                (apply #'make-comint-in-buffer name (generate-new-buffer name) exe nil args))))
        (set-buffer comint-buffer)
        (inf-iex-mode)
        (pop-to-buffer (current-buffer))))))


(provide 'inf-iex)
;;; inf-iex.el ends here
