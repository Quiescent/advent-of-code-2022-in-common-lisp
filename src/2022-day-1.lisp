(defpackage 2022-day-1
  (:use :cl :iterate :cl-ppcre :metabang-bind :trivia :trivia.ppcre)
  (:shadowing-import-from :arrow-macros :->>))
(in-package 2022-day-1)

(defun part-1 ()
  (with-open-file (f (asdf:system-relative-pathname :advent-of-code-2022-in-common-lisp "src/2022-day-1.in"))
    (do ((line (read-line f nil nil) (read-line f nil nil))
         (current 0)
         (most 0))
        ((null line) most)
      (if (string-equal line "")
          (progn
            (setf most (max most current))
            (setf current 0))
          (incf current (read-from-string line))))))

(defun part-2 ()
  (with-open-file (f (asdf:system-relative-pathname :advent-of-code-2022-in-common-lisp "src/2022-day-1.in"))
    (do ((line (read-line f nil nil) (read-line f nil nil))
         (current 0)
         (counts '()))
        ((null line) (apply #'+ (subseq (sort counts #'>) 0 3)))
      (if (string-equal line "")
          (progn
            (push current counts)
            (setf current 0))
          (incf current (read-from-string line))))))
