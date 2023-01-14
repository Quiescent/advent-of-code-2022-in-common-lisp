(defpackage 2022-day-4-pf
  (:use :cl :iterate :cl-ppcre :metabang-bind :trivia :trivia.ppcre)
  (:shadowing-import-from :arrow-macros :->>))
(in-package 2022-day-4-pf)

(defun part-1 ()
  (with-open-file (f (asdf:system-relative-pathname :advent-of-code-2022-in-common-lisp "src/2022-day-4.in"))
    (labels ((recur (acc)
               (bind ((line (read-line f nil nil)))
                 (if (null line)
                     acc
                     (recur (+ acc
                               (match line
                                 ((ppcre "(\\d+)-(\\d+),(\\d+)-(\\d+)"
                                         (read a)
                                         (read b)
                                         (read c)
                                         (read d))
                                  (if (or (and (>= a c)
                                               (<= b d))
                                          (and (>= c a)
                                               (<= d b)))
                                      1
                                      0)))))))))
      (recur 0))))


(defun part-2 ()
  (with-open-file (f (asdf:system-relative-pathname :advent-of-code-2022-in-common-lisp "src/2022-day-4.in"))
    (labels ((recur (acc)
               (bind ((line (read-line f nil nil)))
                 (if (null line)
                     acc
                     (recur (+ acc
                               (match line
                                 ((ppcre "(\\d+)-(\\d+),(\\d+)-(\\d+)"
                                         (read a)
                                         (read b)
                                         (read c)
                                         (read d))
                                  (if (or (and (>= a c)
                                               (<= a d))
                                          (and (>= b c)
                                               (<= b d))
                                          (and (>= c a)
                                               (<= c b))
                                          (and (>= d a)
                                               (<= d b)))
                                      1
                                      0)))))))))
      (recur 0))))
