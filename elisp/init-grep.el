(require 'grep)

(progn
  (add-to-list 'grep-find-ignored-files "*.class")
  (add-to-list 'grep-find-ignored-files "*.min.js")
  (add-to-list 'grep-find-ignored-files "*.min.css")
  (add-to-list 'grep-find-ignored-directories "elpa")
  (add-to-list 'grep-find-ignored-directories "quelpa")
  (add-to-list 'grep-find-ignored-directories "target")
  (add-to-list 'grep-find-ignored-directories "target")
  (add-to-list 'grep-find-ignored-directories ".shadow-cljs")
  (add-to-list 'grep-find-ignored-directories ".cpcache")
  (add-to-list 'grep-find-ignored-directories "dist")
  (add-to-list 'grep-find-ignored-directories "node_modules"))

(provide 'init-grep)
