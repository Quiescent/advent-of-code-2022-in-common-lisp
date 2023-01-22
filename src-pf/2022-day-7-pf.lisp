(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun shadowing-import-from-fset ()
    `(:shadowing-import-from
      :fset
      ,@(let ((symbols nil))
          (do-external-symbols (symbol (find-package 'fset) symbols)
            (push symbol symbols))))))

(defpackage 2022-day-7-pf
  (:use :cl :iterate :cl-ppcre :metabang-bind :trivia :trivia.ppcre)
  #.(shadowing-import-from-fset)
  (:shadowing-import-from :arrow-macros :->>))
(in-package 2022-day-7-pf)

(defun part-1 ()
  (with-open-file (f (asdf:system-relative-pathname :advent-of-code-2022-in-common-lisp "src/2022-day-7.in"))
    (labels ((total (hierarchy)
               (reduce (lambda (acc key value)
                         (declare (ignore key))
                         (if (<= value 100000)
                             (+ acc value)
                             acc))
                       hierarchy
                       :initial-value 0))
             (accumulate (hierarchy path size)
               (if (empty? path)
                   hierarchy
                   (accumulate (with hierarchy path (+ (@ hierarchy path) size))
                               (less-last path)
                               size)))
             (process (hierarchy path)
               (bind ((line (read-line f nil nil)))
                 (if (null line)
                     (total hierarchy)
                     (match line
                       ((ppcre "\\$ cd (.*)"
                               directory)
                        (cond
                          ((equal directory "/")
                           (process hierarchy (seq "/")))
                          ((equal directory "..")
                           (process hierarchy (less-last path)))
                          (t
                           (process hierarchy (with-last path directory)))))
                       ((ppcre "\\$ ls")
                        (process hierarchy path))
                       ((ppcre "dir .*")
                        (process hierarchy path))
                       ((ppcre "([0-9]+) .*"
                               (read size))
                        (process (accumulate hierarchy path size)
                                 path)))))))
      (process (empty-map 0) (seq "/")))))
