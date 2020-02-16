
(defvar m4d-esc-delay 0.01)

;;; Terminal ESC

(defun m4d-esc (map)
  (if (let ((keys (this-single-command-keys)))
        (and (> (length keys) 0)
             (= (aref keys (1- (length keys))) ?\e)
             (sit-for m4d-esc-delay)))
      (prog1 [escape]
        (when defining-kbd-macro
          (end-kbd-macro)
          (setq last-kbd-macro (vconcat last-kbd-macro [escape]))
          (start-kbd-macro t t)))
    map))

(defun m4d--init-esc (frame)
  (let ((term (frame-terminal)))
    (when (terminal-live-p term)
      (let ((esc-map (lookup-key input-decode-map [?\e])))
        (set-terminal-parameter term 'm4d-esc-map esc-map)
        (define-key input-decode-map [?\e] `(menu-item "" ,esc-map :filter ,#'m4d-esc))))))


(provide 'm4d-esc)
