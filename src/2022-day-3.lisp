(defpackage 2022-day-3
  (:use :cl :iterate :cl-ppcre :metabang-bind :trivia :trivia.ppcre)
  (:shadowing-import-from :arrow-macros :->>)
  (:shadowing-import-from :arrow-macros :->))
(in-package 2022-day-3)

(defun priority (char)
  (if (> (char-code char)
         (char-code #\a))
      (1+ (- (char-code char)
             (char-code #\a)))
      (+ 26 (1+ (- (char-code char)
                   (char-code #\A))))))

(defun part-1 ()
  (with-open-file (f (asdf:system-relative-pathname :advent-of-code-2022-in-common-lisp "src/2022-day-3.in"))
    (do ((line (-> (read-line f nil nil) (coerce 'list))
               (-> (read-line f nil nil) (coerce 'list)))
         (sum 0))
        ((null line) sum)
      (incf sum (-> (intersection (subseq line 0 (floor (length line) 2))
                                  (subseq line (floor (length line) 2)))
                  car
                  priority)))))

(defun part-2 ()
  (with-open-file (f (asdf:system-relative-pathname :advent-of-code-2022-in-common-lisp "src/2022-day-3.in"))
    (do* ((line (-> (read-line f nil nil) (coerce 'list))
                (-> (read-line f nil nil) (coerce 'list)))
          (sum 0)
          (counter 0)
          (chars line))
         ((null line) (+ sum #1=(-> (car chars) priority)))
      (if (and (/= 0 counter)
               (= 0 (mod counter 3)))
          (progn
            (incf sum #1#)
            (setf chars line))
          (setf chars (intersection chars line)))
      (incf counter))))
