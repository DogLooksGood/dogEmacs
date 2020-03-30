;;; -*- lexical-binding: t -*-

(require 'cl-lib)

;;; Errors

(defconst pi/error-eol-backslash
  "Line cannot end in a hanging character escaping.")

(defconst pi/error-quote-danger
  "Quotes must balanced inside comment blocks.")

(defconst pi/error-unclosed-quote
  "String is missing a closing quote.")

(defconst pi/error-unclosed-paren
  "Unclosed open-paren.")

(defconst pi/error-unmatched-close-paren
  "Unmatched close-paren.")

(defconst pi/error-leading-close-paren
  "Line cannot lead with a close-paren.")

;;; Faces

(defface parinfer-trail-face
  '((t (:foreground "grey40")))
  "Face for dimming trails."
  :group 'parinfer)

(defface parinfer-error-face
  '((t (:underline "red")))
  "Face for parsing error."
  :group 'parinfer)

(defvar pi/current-indent 0
  "Current indentation.")

(defvar pi/at-indent nil
  "If we are locate at indent of current line.")

(defvar pi/changes nil
  "The changes to made.")

(defvar pi/line-no 0)

(defvar pi/in-string nil
  "If we are in string.")

(defvar pi/in-comment nil
  "If we are in comment.")

(defvar pi/error nil
  "A list of errors.

Has structure of (error . (beg . end)) ")

(defvar pi/openers nil
  "A stack of openers, we will push element in when we meet an opener.

Each element has a structure of (type . point)")

(defvar pi/closers nil
  "A stack of closers, we will push element in when we meet a closer,

Each element has a structure of (type . point)")

(defvar pi/current-trail nil
  "Current tail, has structure of (beg . end)")

(defvar pi/trails nil
  "A list of trails where we can insert or remove parens.

The trail should start with a closer, contains zero or more whitespace and closers, till the end of line. But contains no code characters.
Each element has a structure of (beg . end).
When we apply modifications to buffer, all whitespace in the trails will be deleted.
All closers in the trail will be apply a dim face.")

(defvar pi/indents nil
  "A list of indent to make.

Each element has a structure of (line-number . shift-x)")

(defvar pi/parse-state nil
  "Current state when parsing, can be one of: nil, 'string, 'comment, 'escape.")

(make-variable-buffer-local
 (defvar pi/escape-char nil))

(setq-default pi/escape-char 92)

;;; Debug

(defvar pi/log-buffer-name "*parinfer-log*")

;;; Processing

(defun pi/delete (pos)
  (push pi/changes (cons nil pos)))

(defun pi/insert (ch pos)
  (push pi/changes (cons ch pos)))

(defun pi/init-current-trail (pos)
  "Init current trail at POS."
  (setq pi/current-trail (cons pos pos)))

(defun pi/extend-current-trail ()
  "Extend current trail by one point."
  (setq pi/current-trail
        (cons (car pi/current-trail)
              (1+ (cdr pi/current-trail)))))

(defun pi/handle-indent-begin ()
  (setq pi/line-no (1+ pi/line-no))
  (setq pi/at-indent t))

(defun pi/handle-indent-end ())

(defun pi/replace-current-trail (pos)
  "When we start a new trail at POS to replace the current one."
  (when pi/closers
    (pi/log "replace trail" (list :pos pos :closers pi/closers))
    (dolist (closer pi/closers)
      (if (not pi/openers)
          (progn
            (pi/log "extra closer, remove it." pos)
            (pi/delete pos))
        (-let* (((closer-type . closer-pos) closer)
                (opener (pop pi/openers))
                ((opener-type . opener-pos) opener))
          (if (eq closer-type opener-type)
              (pi/log "match paren" (list :opener opener :closer closer))
            (pi/log "! unmatched close paren" pos)
            (setq pi/error (cons pi/error-unmatched-close-paren
                                 (cons pos pos)))))))
    (setq pi/closers nil))
  (setq pi/current-trail (cons pos pos)))

(defun pi/handle-trail-end (indent)
  "End current trail with current trail."
  ())

(defun pi/process-opener (type pos)
  "Process a opener with `type' at `pos'.
Update openers.
Init current trail at pos.
Set pi/at-indent to nil."
  (pi/replace-current-trail pos)
  (pi/log "opener" (cons type (1- pos)))
  (push (cons type (1- pos)) pi/openers)
  (setq pi/at-indent nil))

(defun pi/process-closer (type pos)
  "Process a closer with `type' at `pos'.
Update openers.
Extend current trail at pos.
Set pi/at-indent to nil."
  (pi/log "closer" (cons type (1- pos)))
  (push (cons type (1- pos)) pi/closers)
  (pi/extend-current-trail)
  (setq pi/at-indent nil))

(defun pi/process-newline (pos)
  "Process a newline at `pos'.
Increament pi/line-no by 1.
Set pi/at-indent to t. "
  (pi/log "newline" pos)
  (pi/handle-indent-begin))

(defun pi/process-whitespace (pos)
  "Process a whitespace at `pos'.
When at indent,
  Increment pi/current-indent by 1.
When not,
  Extend pi/current-trail."
  (cond
   (pi/at-indent (setq pi/current-indent (1+ pi/current-indent)))
   (t
    (pi/extend-current-trail))))

(defun pi/process-delimiter (pos))

(defun pi/process-commenter (pos))

(defun pi/process-code (ch pos)
  "Process code at `pos'.
Init current trail at pos.
Set pi/at-indent to nil."
  (pi/log "code" (cons (char-to-string ch) pos))
  (pi/replace-current-trail pos)
  (setq pi/at-indent nil))

(defun pi/parse-1 (pos)
  "Parse on a single pos."
  (let ((ch (char-before)))
    (assert ch t "pi/parse-1 Error: ch is nil.")
    (if (= ch pi/escape-char)
        (pi/log "escape" 'escape)
      (case ch
        (32  (pi/process-whitespace pos))
        (10  (pi/process-newline pos))
        (40  (pi/process-opener 'round pos))
        (41  (pi/process-closer 'round pos))
        (91  (pi/process-opener 'square pos))
        (93  (pi/process-closer 'square pos))
        (123 (pi/process-opener 'curly pos))
        (125 (pi/process-closer 'curly pos))
        (34  (pi/process-delimiter pos))
        (59  (pi/process-commenter pos))
        (t (pi/process-code ch pos))))))

(defun pi/reset-state ()
  (setq pi/openers nil
        pi/closers nil
        pi/trails nil
        pi/indents nil
        pi/at-indent t
        pi/line-no 0
        pi/error nil
        pi/current-indent 0
        pi/current-trail nil
        pi/changes nil))

(defun pi/parse ()
  (pi/reset-state)
  (save-mark-and-excursion
    (goto-char (point-min))
    (while (and (not (= (point) (point-max)))
                (not pi/error))
      (forward-char 1)
      (pi/parse-1 (point)))))

;;; Highlight

(defun pi/mark-error ()
  (remove-overlays (point-min) (point-max) 'name 'parinfer-error)
  (when pi/error
    (-let* (((error-message . (beg . end)) pi/error)
            (ov (make-overlay beg end)))
      (overlay-put ov 'face 'parinfer-error-face)
      (overlay-put ov 'name 'parinfer-error))))

(defun pi/mark-trails ()
  "Mark trails for current buffer."
  (remove-overlays (point-min) (point-max) 'name 'parinfer-trail)
  (dolist (p pi/trails)
    (when p
      (-let (((beg . end) p)
             (ov (make-overlay beg end)))
        (overlay-put ov 'face 'parinfer-trail-face)
        (overlay-put ov 'name 'parinfer-trail)))))

(defun pi/log (pre msg)
  (with-current-buffer (get-buffer-create pi/log-buffer-name)
    (insert (format "[ LN:%s ]%s -> %s\n" pi/line-no pre msg))))

(defun pi/log-state ()
  (pi/log "Openers" pi/openers)
  (pi/log "Closers" pi/closers)
  (pi/log "Trails" pi/trails)
  (pi/log "Changes" pi/changes)
  (pi/log "Error" pi/error)
  (when (window-live-p (get-buffer-window pi/log-buffer-name))
    (with-selected-window (get-buffer-window pi/log-buffer-name)
      (goto-char (point-max)))))

(defun pi/log-clr ()
  (with-current-buffer (get-buffer-create pi/log-buffer-name)
    (delete-region (point-min) (point-max))
    (text-mode)))

(defun pi/process-buffer ()
  (interactive)
  (pi/log-clr)
  (pi/parse)
  (pi/mark-trails)
  (pi/mark-error)
  (pi/log-state))

(global-set-key (kbd "C-z") #'pi/process-buffer)

(provide 'parinfer)
