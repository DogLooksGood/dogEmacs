;

(defun user/rust-semicolon ()
  "Will insert a semicolon if we are at the end of line,
otherwise will insert a colon."
  (interactive)
  (if (equal 59 (char-before))
      (progn
	(delete-char -1)
	(insert "::"))
    (if (and (or (nth 3 (syntax-ppss))
		 (nth 4 (syntax-ppss))
		 (let ((ln (buffer-substring-no-properties
			    (line-beginning-position)
			    (line-end-position))))
		   (and (string-match-p "[=\.)}]\\|\\( return \\)\\|\\( break \\)" ln)
			(equal (point) (line-end-position))))))
	(call-interactively #'self-insert-command)
      (insert ":"))))

(defun user/rust-minus ()
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
  :ensure t
  :bind
  (:map
   rust-mode-map
   ("-" . 'user/rust-minus)
   (";" . 'user/rust-semicolon)
   ("C-c C-c" . 'rust-run)
   ("C-c C-p" . 'rust-compile)
   ("C-c C-t" . 'rust-test)
   :map
   compilation-mode-map
   ("n" . 'forward-line)
   ("p" . 'previous-line)
   ("f" . 'forward-char)
   ("b" . 'backward-char))
  :init
  (add-hook 'rust-mode-hook 'smartparens-mode))

(provide 'the-rust)
