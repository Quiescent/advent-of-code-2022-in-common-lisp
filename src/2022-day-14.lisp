(defpackage 2022-day-14
  (:use :cl :iterate :cl-ppcre :metabang-bind :trivia :trivia.ppcre)
  (:shadowing-import-from :arrow-macros :->>))
(in-package 2022-day-14)

(defun bounds (lines)
  (iter outer
    (for line in lines)
    (iter
      (for (x y) in line)
      (in outer
          (minimizing x into min-x)
          (minimizing y into min-y)
          (maximizing x into max-x)
          (maximizing y into max-y)))
    (finally
     (return-from outer (list min-x min-y max-x max-y)))))

(defun fill-lines (lines plane)
  (iter
    (for line in lines)
    (iter
      (for (x y) in line)
      (for p-x previous x)
      (for p-y previous y)
      (when (or (null p-x)
                (null p-y))
        (next-iteration))
      (if (/= x p-x)
          (iter
            (for xx from (min p-x x) to (max p-x x))
            (setf (aref plane xx y) t))
          (iter
            (for yy from (min p-y y) to (max p-y y))
            (setf (aref plane x yy) t))))))

(defun print-plane (plane min-x min-y)
  (bind (((max-x max-y) (array-dimensions plane)))
    (iter
      (for y from min-y below max-y)
      (iter
        (for x from min-x below max-x)
        (if (aref plane x y)
            (format t "#")
            (format t ".")))
      (format t "~%"))))

(defun part-1 ()
  (bind ((input (with-open-file (f (asdf:system-relative-pathname :advent-of-code-2022-in-common-lisp "src/2022-day-14.in"))
                 (do ((line (read-line f nil nil) (read-line f nil nil))
                      (lines (list)))
                     ((null line) lines)
                   (push (->> (split " -> " line)
                           (mapcar (lambda (coord) (mapcar #'read-from-string
                                                           (split "," coord) ))))
                         lines))))
         ((min-x min-y max-x max-y) (bounds input))
         (plane (make-array (list (1+ max-x) (1+ max-y)) :initial-element nil)))
    (fill-lines input plane)
    ))

(defun part-2 ()
  (let ((input (with-open-file (f (asdf:system-relative-pathname :advent-of-code-2022-in-common-lisp "src/2022-day-14.in"))
                 )))
    ))
