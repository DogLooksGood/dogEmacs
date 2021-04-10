;;; -*- lexical-binding: t -*-

(straight-use-package 'dash)

(require 'dash)
(require 'subr-x)

(defvar-local +project-name-cache nil
  "Cache for current project name.")

(defun +in-string-p ()
  "Returns non-nil if inside string, else nil.
Result depends on syntax table's string quote character."
  (interactive)
  (or (nth 3 (syntax-ppss))
      (member 'font-lock-string-face
              (text-properties-at (point)))))

(defun +in-comment-p ()
  "Returns non-nil if inside comment, else nil.
Result depends on syntax table's comment character."
  (interactive)
  (nth 4 (syntax-ppss)))

(defun +smart-file-name ()
  "Get current file name, if we are in project, the return relative path to the project root, otherwise return absolute file path.
This function is slow, so we have to use cache."
  (let ((vc-dir (vc-root-dir))
        (bfn (buffer-file-name (current-buffer))))
    (cond
     ((and bfn vc-dir)
      (file-relative-name bfn vc-dir))
     (bfn bfn)
     (t (buffer-name)))))

(defmacro +measure-time-1 (&rest body)
  "Measure the time it takes to evaluate BODY."
  `(let ((time (current-time)))
     ,@body
     (message "%.03fms"
              (* 1000 (float-time (time-since time))))))

(defmacro +measure-time (&rest body)
  "Measure the time it takes to evalutae BODY, repeat 10 times."
  `(let ((time (current-time))
         (n 10))
     (dotimes (_ n),@body)
     (message "%.03fms"
              (/ (* (float-time (time-since time)) 1000) n))))

(defface +modeline-dim-face
  '((((class color) (background dark))
     (:foreground "grey50"))
    (((class color) (background light))
     (:foreground "grey50")))
  "Dim face in mode-line")

(defvar-local +smart-file-name-with-propertize-cache nil
  "Cache for performance, is a cons of (buffer-name . cached-value).")

(defun +smart-file-name-cached ()
  (when +smart-file-name-with-propertize-cache
    (-let (((buf-name p f) +smart-file-name-with-propertize-cache))
      (when (string-equal buf-name (buffer-file-name))
        (let ((face (cond
                     ((buffer-modified-p) 'font-lock-string-face)
                     (buffer-read-only 'font-lock-comment-face)
                     (t nil))))
          (concat (if p (propertize p 'face '+modeline-dim-face) "")
                  (propertize f 'face face)))))))

(defun +smart-file-name-with-propertize ()
  (if-let ((bfn (buffer-file-name)))
      (if-let ((cached (+smart-file-name-cached)))
          cached
        (let ((vc-dir (vc-root-dir)))
          (if vc-dir
              (let* ((fname (file-relative-name bfn vc-dir))
                     (p (file-name-directory fname))
                     (f (file-name-nondirectory fname)))
                (setq-local +smart-file-name-with-propertize-cache (list (buffer-file-name) p f))
                (+smart-file-name-cached))
            (let* ((p (file-name-directory bfn))
                   (f (file-name-nondirectory bfn)))
              (setq-local +smart-file-name-with-propertize-cache (list (buffer-file-name) p f))
              (+smart-file-name-cached)))))
    (buffer-name)))

(defun +file-vc-state-with-propertize ()
  (when-let ((sym (vc-state (buffer-file-name (current-buffer)))))
    (format "%s" sym)))

(defun +vc-branch ()
  (car (vc-git-branches)))

(defun +project-name ()
  "Get project name, which is used in title format."
  (cond
   (+project-name-cache +project-name-cache)
   ((project-current)
    (setq-local +project-name-cache
                (-> (project-root (project-current))
                    (string-trim-right "/")
                    (file-name-base))))
   (t (setq-local +project-name-cache ""))))

(defun +make-silent (func &rest args)
  (cl-letf (((symbol-function 'message)
             (lambda (&rest args) nil)))
    (apply func args)))

;;; Case transform

(defun +to-pascal-case (s)
  (let* ((words (split-string s "-\\|_"))
         (capwords (mapcar #'capitalize words)))
    (string-join capwords "")))

(defun +color-blend (c1 c2 alpha)
  "Blend two colors C1 and C2 with ALPHA.
C1 and C2 are hexidecimal strings.
ALPHA is a number between 0.0 and 1.0 which corresponds to the
influence of C1 on the result."
  (ignore-errors
    (apply #'(lambda (r g b)
               format "#%02x%02x%02x"
               (ash r -8)
               (ash g -8)
               (ash b -8))
           (cl-mapcar
            (lambda (x y)
              (round (+ (* x alpha) (* y (- 1 alpha)))))
            (color-values c1) (color-values c2)))))

(defun +set-no-other-window ()
  (set-window-parameter (first (window-list)) 'no-other-window t))

(provide 'init-util)
