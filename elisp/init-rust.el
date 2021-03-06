(defun +rust-whitespace ()
  (interactive)
  (if (or (nth 3 (syntax-ppss))
          (nth 4 (syntax-ppss)))
      (call-interactively #'self-insert-command)
    (if (equal 59 (char-before))
        (progn
          (delete-char -1)
          (insert ": "))
      (call-interactively #'self-insert-command))))

(defun +rust-lessthan ()
  (interactive)
  (if (or (nth 3 (syntax-ppss))
          (nth 4 (syntax-ppss)))
      (call-interactively #'self-insert-command)
    (if (equal 32 (char-before))
        (insert "<")
      (call-interactively #'self-insert-command))))

(defun +rust-semicolon ()
  (interactive)
  (cond
   ((or (nth 3 (syntax-ppss))
        (nth 4 (syntax-ppss)))
    (call-interactively #'self-insert-command))

   ((equal 58 (char-before))
    (insert ":"))

   ((equal 59 (char-before))
    (progn
      (delete-char -1)
      (insert "::")))

   ((not (equal (point) (line-end-position)))
    (insert ":"))

   (t (call-interactively #'self-insert-command))))

(defun +rust-minus ()
  "Will insert a minus if we are after whitespace and not at the indentation,otherwise will insert a underscore."
  (interactive)
  (if (and (or (nth 3 (syntax-ppss))
               (nth 4 (syntax-ppss))
               (and (equal 32 (char-before))
		    (let ((pos (point)))
		      (not (equal pos
				  (save-mark-and-excursion
				    (back-to-indentation)
				    (point))))))))
      (call-interactively #'self-insert-command)
    (insert "_")))

(use-package rust-mode
  :bind
  (:map
   rust-mode-map
   ("-" . '+rust-minus)
   ("<" . '+rust-lessthan)
   ("SPC" . '+rust-whitespace)
   (";" . '+rust-semicolon)))

(use-package cargo
  :bind
  (:map
   rust-mode-map
   ("C-c C-c" . 'cargo-process-run)
   ("C-c C-t t" . 'cargo-process-current-test)
   ("C-c C-t f" . 'cargo-process-current-file-tests)
   ("C-c C-t p" . 'cargo-process-test)
   ("C-c C-k" . 'cargo-process-check)))

(provide 'init-rust)
