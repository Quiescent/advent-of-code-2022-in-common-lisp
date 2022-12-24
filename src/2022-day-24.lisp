(defpackage 2022-day-24
  (:use :cl :iterate :cl-ppcre :metabang-bind :trivia :trivia.ppcre)
  (:shadowing-import-from :arrow-macros :->>))
(in-package 2022-day-24)

(defun part-1 ()
  (bind ((((x . y) . blizzards) (fresh-grid)))
   (shortest-path x y blizzards)))

(defun tick-blizzards (x-dim y-dim blizzards)
  (iter
    (for (position . direction) in blizzards)
    (for new-pos = (+ position direction))
    (for x = (realpart new-pos))
    (for y = (imagpart new-pos))
    (cond
      ((>= x x-dim) (collecting (cons (complex 0          y)          direction)))
      ((< x 0)      (collecting (cons (complex (1- x-dim) y)          direction)))
      ((>= y y-dim) (collecting (cons (complex x          0)          direction)))
      ((< y 0)      (collecting (cons (complex x          (1- y-dim)) direction)))
      (t            (collecting (cons new-pos                         direction))))))

(defun shortest-path (x-dim y-dim blizzards)
  )

(defun fresh-grid ()
  (iter outer
    (with lines = (with-open-file (f (asdf:system-relative-pathname :advent-of-code-2022-in-common-lisp "src/2022-day-24.in"))
                    (streams-lines f)))
    (with y-dim = (- (length lines) 2))
    (with x-dim = (- (->> (car lines)
                       length)
                     2))
    (for line in lines)
    (for y from 0)
    (iter
      (for char in-string line)
      (for x from 0)
      (when (and (not (eq char #\#))
                 (not (eq char #\.)))
        (in outer (collecting (cons (complex x y)
                                    (cond
                                      ((eq char #\>) #c(1 0))
                                      ((eq char #\<) #c(-1 0))
                                      ((eq char #\^) #c(0 -1))
                                      ((eq char #\v) #c(0 1))))
                              into blizzards))))
    (finally (return-from outer
               (cons (cons x-dim y-dim)
                     blizzards)))))

(defun streams-lines (f)
  (do ((line (read-line f nil nil) (read-line f nil nil))
       (lines))
      ((null line) (nreverse lines))
    (push line lines)))

(defun part-2 ()
  (let ((input (with-open-file (f (asdf:system-relative-pathname :advent-of-code-2022-in-common-lisp "src/2022-day-24.in"))
                 )))
    ))
