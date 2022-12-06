(defpackage 2022-day-6
  (:use :cl :iterate :cl-ppcre :metabang-bind :trivia :trivia.ppcre)
  (:shadowing-import-from :arrow-macros :->>))
(in-package 2022-day-6)

(defun part-1 ()
  (let ((line (with-open-file (f (asdf:system-relative-pathname :advent-of-code-2022-in-common-lisp "src/2022-day-6.in"))
                (coerce (read-line f nil nil) 'list))))
    (iter
      (for char in line)
      (for p-char previous char)
      (for pp-char previous p-char)
      (for ppp-char previous pp-char)
      (for i from 0)
      (when (and char p-char pp-char ppp-char
                 (= (length (remove-duplicates (list char p-char pp-char ppp-char))) 4))
        (return (1+ i))))))

(defun part-2 ()
  (let ((line (with-open-file (f (asdf:system-relative-pathname :advent-of-code-2022-in-common-lisp "src/2022-day-6.in"))
                (coerce (read-line f nil nil) 'list))))
    (iter
      (for i from 0)
      (when (= (length (remove-duplicates (subseq line i (+ i 14)))) 14)
        (return (+ i 14))))))
