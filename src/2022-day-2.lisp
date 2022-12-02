(defpackage 2022-day-2
  (:use :cl :iterate :cl-ppcre :metabang-bind :trivia :trivia.ppcre)
  (:shadowing-import-from :arrow-macros :->>))
(in-package 2022-day-2)

(defun part-1 ()
  (with-open-file (f (asdf:system-relative-pathname :advent-of-code-2022-in-common-lisp "src/2022-day-2.in"))
    (do ((line (read-line f nil nil) (read-line f nil nil))
         (score 0))
        ((null line) score)
      (bind (((a b) (mapcar #'read-from-string (split " " line))))
        (incf score
              (case a
                (a (case b
                     (x (+ 1 3))
                     (y (+ 2 6))
                     (z 3)))
                (b (case b
                     (x 1)
                     (y (+ 2 3))
                     (z (+ 3 6))))
                (c (case b
                     (x (+ 1 6))
                     (y 2)
                     (z (+ 3 3))))))))))
;; A for Rock, B for Paper, and C for Scissors

(defun part-2 ()
  (with-open-file (f (asdf:system-relative-pathname :advent-of-code-2022-in-common-lisp "src/2022-day-2.in"))
    (do ((line (read-line f nil nil) (read-line f nil nil))
         (score 0))
        ((null line) score)
      (bind (((a b) (mapcar #'read-from-string (split " " line))))
        (incf score
              (case a
                (a (case b
                     (x 3)
                     (y (+ 1 3))
                     (z (+ 2 6))))
                (b (case b
                     (x 1)
                     (y (+ 2 3))
                     (z (+ 3 6))))
                (c (case b
                     (x 2)
                     (y (+ 3 3))
                     (z (+ 1 6))))))))))
