(defpackage 2022-day-4
  (:use :cl :iterate :cl-ppcre :metabang-bind :trivia :trivia.ppcre)
  (:shadowing-import-from :arrow-macros :->>))
(in-package 2022-day-4)

(defun part-1 ()
  (with-open-file (f (asdf:system-relative-pathname :advent-of-code-2022-in-common-lisp "src/2022-day-4.in"))
    (do ((line (read-line f nil nil) (read-line f nil nil))
         (count 0))
        ((null line) count)
      (match line
        ((ppcre "(\\d+)-(\\d+),(\\d+)-(\\d+)"
                (read a)
                (read b)
                (read c)
                (read d))
         (when (or (and (>= a c)
                        (<= b d))
                   (and (>= c a)
                        (<= d b)))
           (incf count)))))))

(defun part-2 ()
  (with-open-file (f (asdf:system-relative-pathname :advent-of-code-2022-in-common-lisp "src/2022-day-4.in"))
    (do ((line (read-line f nil nil) (read-line f nil nil))
         (count 0))
        ((null line) count)
      (match line
        ((ppcre "(\\d+)-(\\d+),(\\d+)-(\\d+)"
                (read a)
                (read b)
                (read c)
                (read d))
         (when (or (and (>= a c)
                        (<= a d))
                   (and (>= c a)
                        (<= c b)))
           (incf count)))))))
