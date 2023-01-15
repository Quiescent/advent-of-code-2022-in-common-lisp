(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun shadowing-import-from-fset ()
    `(:shadowing-import-from
      :fset
      ,@(let ((symbols nil))
          (do-external-symbols (symbol (find-package 'fset) symbols)
            (push symbol symbols))))))

(defpackage 2022-day-6-pf
  (:use :cl :iterate :cl-ppcre :metabang-bind :trivia :trivia.ppcre)
  #.(shadowing-import-from-fset)
  (:shadowing-import-from :arrow-macros :->>))
(in-package 2022-day-6-pf)

(defun part-1 ()
  (with-open-file (f (asdf:system-relative-pathname :advent-of-code-2022-in-common-lisp "src-pf/2022-day-6.in"))
    (labels ((recur (pos queue)
               (bind ((char (read-char f nil nil))
                      (new-queue (with-first queue char)))
                 (cond
                   ((null char) nil)
                   ((= (size (convert 'set new-queue)) 4) pos)
                   (t (recur (1+ pos)
                             (if (>= (size new-queue) 4)
                                 (less-last new-queue)
                                 new-queue)))))))
      (recur 1 (empty-seq)))))

(defun part-2 ()
  (with-open-file (f (asdf:system-relative-pathname :advent-of-code-2022-in-common-lisp "src-pf/2022-day-6.in"))
    (labels ((recur (pos queue)
               (bind ((char (read-char f nil nil))
                      (new-queue (with-first queue char)))
                 (cond
                   ((null char) nil)
                   ((= (size (convert 'set new-queue)) 14) pos)
                   (t (recur (1+ pos)
                             (if (>= (size new-queue) 14)
                                 (less-last new-queue)
                                 new-queue)))))))
      (recur 1 (empty-seq)))))
