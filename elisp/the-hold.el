(defvar user/last-self-insert-char nil)
(defvar user/last-self-insert-time nil)
(defvar user/count-same-key 0)
(defvar user/upcase-trigger-count 3)
(defvar user/upcase-max-interval 0.16)

(defun user/upcase-previous-char ()
  (let ((ch (char-before)))
    (when ch
      (cond
       ((= ch 64) (delete-char -1) (insert "^"))
       ((= ch 47) (delete-char -1) (insert "?"))
       ((= ch 124) (delete-char -1) (insert "|"))
       ((= ch 35) (delete-char -1) (insert "`"))
       ((= ch 44) (delete-char -1) (insert "<"))
       ((= ch 46) (delete-char -1) (insert ">"))
       ((= ch 59) (delete-char -1) (insert ":"))
       ((= ch 36) (delete-char -1) (insert "~"))
       ((= ch 38) (delete-char -1) (insert "%"))
       (t (upcase-char -1))))))

(defun user/after-self-insert ()
  (if (and user/last-self-insert-char
           user/last-self-insert-time
           (not (or (equal user/last-self-insert-char 'return)
                    (equal user/last-self-insert-char 32)))
           ;; we are not first time
           (> user/count-same-key 0)
           ;; press the same key
           (equal user/last-self-insert-char last-input-event)
           ;; in a short duration
           (time-less-p (current-time)
                        (time-add user/last-self-insert-time
                                  user/upcase-max-interval)))
      (progn
        (setq user/count-same-key (1+ user/count-same-key))
        (cond
         ((> user/count-same-key user/upcase-trigger-count)
          (delete-char -1))

         ((= user/count-same-key user/upcase-trigger-count)
          (backward-delete-char (1- user/upcase-trigger-count))
          (user/upcase-previous-char))))
    (setq user/count-same-key 1))
  (setq user/last-self-insert-char last-input-event
        user/last-self-insert-time (current-time)))

(add-hook 'post-self-insert-hook 'user/after-self-insert)

(provide 'the-hold)










(provide 'the-hold)
