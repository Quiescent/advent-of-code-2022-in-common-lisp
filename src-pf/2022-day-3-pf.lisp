(defpackage 2022-day-3-pf
  (:use :cl :metabang-bind)
  (:shadowing-import-from :fset :intersection :convert :set :seq :arb)
  (:shadowing-import-from :arrow-macros :->>))
(in-package 2022-day-3-pf)

(defun score (char)
  (bind ((code-point (char-code char)))
    (cond
      ((and (>= code-point (char-code #\a))
            (<= code-point (char-code #\z)))
       (1+ (- code-point (char-code #\a))))
      ((and (>= code-point (char-code #\A))
            (<= code-point (char-code #\Z)))
       (+ 27 (- code-point (char-code #\A)))))))

(defun part-1 ()
  (with-open-file (f (asdf:system-relative-pathname :advent-of-code-2022-in-common-lisp "src-pf/2022-day-3.in"))
    (labels ((recur (acc)
               (bind ((line (read-line f nil nil)))
                 (if (null line)
                     acc
                     (bind ((mid (floor (length line) 2))
                            (a (subseq line 0 mid))
                            (b (subseq line mid)))
                       (recur (+ acc
                                 (->> (intersection (convert 'set a)
                                                    (convert 'set b))
                                   arb
                                   score))))))))
      (recur 0))))

(defun part-2 ()
  (with-open-file (f (asdf:system-relative-pathname :advent-of-code-2022-in-common-lisp "src-pf/2022-day-3.in"))
    (labels ((group-score (group)
               (->> (reduce #'intersection group)
                 arb
                 score))
             (recur (group acc)
               (bind ((line (read-line f nil nil)))
                 (cond
                   ((null line) (+ acc (if (= (length group) 3)
                                           (group-score group)
                                           0)))
                   ((= (length group) 3)
                    (recur (list (convert 'set line))
                           (+ acc (group-score group))))
                   (t (recur (cons (convert 'set line) group)
                             acc))))))
      (recur nil 0))))
