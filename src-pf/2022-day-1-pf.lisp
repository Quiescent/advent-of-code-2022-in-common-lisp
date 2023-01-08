(defpackage 2022-day-1-pf
  (:use :cl :metabang-bind)
  (:shadowing-import-from :fset :with-first :reduce :sort :subseq :empty-seq)
  (:shadowing-import-from :arrow-macros :->))
(in-package 2022-day-1-pf)

(defun part-1 ()
  (with-open-file (f (asdf:system-relative-pathname :advent-of-code-2022-in-common-lisp "src/2022-day-1.in"))
    (labels ((recur (current acc)
               (bind ((line (read-line f nil nil)))
                 (cond
                   ((null line)     acc)
                   ((equal line "") (recur 0 (max acc current)))
                   (t               (recur (+ current (read-from-string line))
                                           acc))))))
      (recur 0 most-negative-fixnum))))

(defun part-2 ()
  (with-open-file (f (asdf:system-relative-pathname :advent-of-code-2022-in-common-lisp "src/2022-day-1.in"))
    (labels ((recur (current acc)
               (bind ((line (read-line f nil nil)))
                 (cond
                   ((null line)     acc)
                   ((equal line "") (recur 0 (with-first acc current)))
                   (t               (recur (+ current (read-from-string line))
                                           acc))))))
      (reduce #'+
              (-> (recur 0 (empty-seq))
                (sort #'>)
                (subseq 0 3))))))
