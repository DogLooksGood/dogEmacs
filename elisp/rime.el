;;; Example Config:
;;;
;;;
;;; I. If you don't use any modal edit.
;;;
;;;   (global-set-key (kbd "C-\\") 'rime-toggle)
;;;
;;; II. If you use modal edit.
;;;
;;;   a) You have to need a predicate function to tell if rime should be enable in this mode.
;;;
;;;   (defun my-predicate-fn () some-mode)
;;;
;;;   (setq rime--enable-predicates '(my-predicate-fn))
;;;
;;;   b) You need add `rime-update-input-method-state' to the hooks those get called when mode toggling.
;;;
;;;   (add-hook 'some-mode-hook 'rime-update-input-method-state)
;;;

(defface rime-preedit-face
  '((((class color) (background dark))
     (:underline t))
    (((class color) (background light))
     (:underline t)))
  "Face for preedit string"
  :group 'rime)

(defface rime-code-face
  '((((class color) (background dark))
     (:inherit hl-line))
    (((class color) (background light))
     (:inherit hl-line)))
  "Face for code in minibuffer"
  :group 'rime)

(defvar rime--enable-predicates t)
(defvar rime--preedit-overlay nil)
(defvar rime--backspace-fallback nil)
(defvar rime--return-fallback nil)
(defvar rime--enable nil)
(defvar rime--show-candidate t)
(defvar rime--prev-preedit nil
  "Record the previous preedit, to prevent empty candidate clear the composition.")

(make-variable-buffer-local 'rime--preedit-overlay)
(make-variable-buffer-local 'rime--backspace-fallback)

(defun rime--should-enable-p ()
  (or (equal rime--enable-predicates t)
      (seq-find 'funcall rime--enable-predicates)))

(defun rime--show-candidates ()
  (when rime--show-candidate
    (let* ((context (liberime-get-context))
           (candidates (alist-get 'candidates (alist-get 'menu context)))
           (preedit (alist-get 'preedit (alist-get 'composition context)))
           (idx 1)
           (result ""))
      (when context
        (setq result
              (concat result (format "%s ┃ " preedit)))
        (dolist (c candidates)
          (setq result
                (concat result (format "%d. %s " idx c)))
          (setq idx (1+ idx))))
      (message result))))

(defun rime--clear-overlay ()
  (when (overlayp rime--preedit-overlay)
    (delete-region (overlay-start rime--preedit-overlay) (overlay-end rime--preedit-overlay))
    (delete-overlay rime--preedit-overlay)
    (setq rime--preedit-overlay nil)))

(defun rime--display-preedit ()
  (let ((preedit (alist-get 'commit-text-preview (liberime-get-context))))
    ;; Always delete the old overlay.
    (rime--clear-overlay)
    ;; Create the new preedit
    (when preedit
      (let ((beg (point)))
        (insert preedit)
        (setq rime--preedit-overlay (make-overlay beg (point)))
        (overlay-put rime--preedit-overlay 'face 'rime-preedit-face)))))

(defun rime--backspace ()
  (interactive)
  (if (not (rime--should-enable-p))
      (when rime--backspace-fallback
        (call-interactively rime--backspace-fallback))
    (let ((context (liberime-get-context)))
      (if (not context)
          (call-interactively rime--backspace-fallback)
        (liberime-process-key 65288)
        (rime--show-candidates)
        (rime--display-preedit)))))

(defun rime--return ()
  (interactive)
  (if (not (rime--should-enable-p))
      (when rime--return-fallback
        (call-interactively rime--return-fallback))
    (liberime-clear-composition)
    (rime--show-candidates)
    (rime--clear-overlay)
    (call-interactively rime--return-fallback)))

(defun rime-input-method (key)
  (if (not (rime--should-enable-p))
      (list key)
    (liberime-process-key key)
    (with-silent-modifications
      (let* ((context (liberime-get-context))
             (preedit (thread-last context
                        (alist-get 'composition)
                        (alist-get 'preedit)))
             (commit (liberime-get-commit)))
        (unwind-protect
            (cond
             ((and (not context) (not commit) (not preedit))
              (if rime--prev-preedit
                  (progn
                    (liberime-clear-composition)
                    (dolist (c (mapcar 'identity rime--prev-preedit))
                      (liberime-process-key c)
                      (rime--show-candidates)
                      (rime--display-preedit))
                    (setq preedit
                          (thread-last (liberime-get-context)
                            (alist-get 'composition)
                            (alist-get 'preedit))))
                (liberime-clear-composition)
                (list key)))
             (commit
              (rime--clear-overlay)
              (mapcar 'identity commit))
             (t (rime--show-candidates)
                (rime--display-preedit)))
          (setq rime--prev-preedit preedit))))))

(defun rime-update-input-method-state ()
  (if (and (rime--should-enable-p) rime--enable)
      (set-input-method 'rime)
    (set-input-method nil)))

(defun rime-toggle ()
  (interactive)
  (setq rime--enable (not rime--enable))
  (rime-update-input-method-state)
  (message (if rime--enable "Rime enable" "Rime disable")))

(defun rime-select-schema ()
  (interactive)
  (let* ((schema-list (liberime-get-schema-list))
         (schema-names (mapcar 'cdr schema-list))
         (schema-name (completing-read "Schema: " schema-names))
         (schema (thread-last schema-list
                   (seq-find (lambda (s)
                               (message "%s %s" (cdr s) schema-name)
                               (equal (cadr s) schema-name)))
                   (car))))
    (message "Rime schema: %s" schema-name)
    (liberime-select-schema schema)))

(defun rime-activate (name)
  (interactive)
  (setq input-method-function 'rime-input-method
        deactivate-current-input-method-function #'rime-deactivate)
  (liberime-clear-composition)
  (setq-local rime--backspace-fallback (key-binding (kbd "DEL")))
  (setq-local rime--return-fallback (key-binding (kbd "RET")))
  (setq rime--prev-preedit nil)
  (rime-mode 1)
  (add-hook 'post-self-insert-hook 'rime--display-preedit nil t)
  (add-hook 'post-self-insert-hook 'rime--show-candidates nil t))

(defun rime-deactivate ()
  "When quit the input method, we preserve the preedit, remove the overlay."
  (liberime-clear-composition)
  (when (overlayp rime--preedit-overlay)
    (delete-overlay rime--preedit-overlay)
    (setq rime--preedit-overlay nil))
  (setq rime--prev-preedit nil)
  (rime-mode -1)
  (remove-hook 'post-self-insert-hook 'rime--display-preedit)
  (remove-hook 'post-self-insert-hook 'rime--show-candidates))

(defvar rime-mode-map nil)
(setq rime-mode-map
      (let ((keymap (make-sparse-keymap)))
        (define-key keymap (kbd "DEL") 'rime--backspace)
        (define-key keymap (kbd "RET") 'rime--return)
        keymap))

(defun rime-lighter ()
  (if rime--enable
      " Rime"
    ""))

(define-minor-mode rime-mode
  "Provide rime input method specific keybindings."
  nil
  nil
  rime-mode-map)
(register-input-method "rime" "euc-cn" 'rime-activate "中")

(setq default-input-method 'rime)

(provide 'rime)
