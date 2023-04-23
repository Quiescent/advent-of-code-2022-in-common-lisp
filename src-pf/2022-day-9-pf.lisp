(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun shadowing-import-from-fset ()
    `(:shadowing-import-from
      :fset
      ,@(let ((symbols nil))
          (do-external-symbols (symbol (find-package 'fset) symbols)
            (push symbol symbols))))))

(defpackage 2022-day-9-pf
  (:use :cl :iterate :cl-ppcre :metabang-bind :trivia :trivia.ppcre)
  #.(shadowing-import-from-fset)
  (:shadowing-import-from :arrow-macros :->>)
  (:shadowing-import-from :arrow-macros :->))
(in-package 2022-day-9-pf)

(defun read-moves (f)
  (labels ((recur ()
             (bind ((line (read-line f nil nil)))
               (unless (null line)
                 (cons (->> (format nil "(~a)" line)
                         read-from-string)
                       (recur))))))
    (recur)))

(defun decode-dir (dir)
  (case dir
    (U #c(0 -1))
    (D #c(0 1))
    (L #c(-1 0))
    (R #c(1 0))))

(defun count-tail-positions (moves)
  (labels ((clip (x)
             (cond
               ((> x 0)  1)
               ((< x 0) -1)
               (t        0)))
           (clipped-diff (head tail)
             (bind ((diff (- head tail))
                    (x (realpart diff))
                    (y (imagpart diff)))
               (cond
                 ((> x 1)  (complex 1 (clip y)))
                 ((< x -1) (complex -1 (clip y)))
                 ((> y 1)  (complex (clip x) 1))
                 ((< y -1) (complex (clip x) -1))
                 (t       0))))
           (move-head (tails head tail dir amt)
             (if (<= amt 0)
                 (list head tail tails)
                 (bind ((new-head (+ head (decode-dir dir)))
                        (new-tail (+ tail (clipped-diff new-head tail))))
                   (move-head (with tails new-tail)
                              new-head
                              new-tail
                              dir
                              (1- amt)))))
           (recur (acc head tail remaining)
             (if (null remaining)
                 (size acc)
                 (bind (((new-head new-tail tails)
                         (move-head (set)
                                    head
                                    tail
                                    (caar remaining)
                                    (cadar remaining))))
                   (recur (union acc tails)
                          new-head
                          new-tail
                          (cdr remaining))))))
    (recur (set)
           #c(0 0)
           #c(0 0)
           moves)))

(defun part-1 ()
  (with-open-file (f (asdf:system-relative-pathname :advent-of-code-2022-in-common-lisp "src/2022-day-9.in"))
    (->> (read-moves f)
      count-tail-positions)))

(defun step-rope (knots move)
  (labels
      ((clip (x)
         (cond
           ((> x 0)  1)
           ((< x 0) -1)
           (t        0)))
       (clipped-diff (head tail)
         (bind ((diff (- head tail))
                (x (realpart diff))
                (y (imagpart diff)))
           (cond
             ((> x 1)  (complex 1 (clip y)))
             ((< x -1) (complex -1 (clip y)))
             ((> y 1)  (complex (clip x) 1))
             ((< y -1) (complex (clip x) -1))
             (t       0))))
       (recur (acc remaining)
         (if (empty? remaining)
             acc
             (bind ((head (first remaining))
                    (tail (less-first remaining))
                    (prev (last acc)))
               (recur (with-last acc (+ head (clipped-diff prev head)))
                      tail)))))
    (recur (seq (+ (first knots) (decode-dir move)))
           (less-first knots))))

(defun run-moves (moves)
  (bind ((init-knots (iter
                       (for i from 0 below 10)
                       (reducing (complex 0 0)
                                 by #'with-last
                                 :initial-value (seq)))))
    (labels ((step-move (tails dir count knots)
               (if (= count 0)
                   (cons tails knots)
                   (bind ((new-knots (step-rope knots dir))
                          (new-tails (with tails (last new-knots))))
                     (step-move new-tails dir (1- count) new-knots))))
             (recur (tails rem-moves knots)
               (if (null rem-moves)
                   tails
                   (bind ((((dir count) . rem) rem-moves)
                          ((new-tails . new-knots) (step-move tails dir count knots)))
                     (recur new-tails rem new-knots)))))
      (recur (set) moves init-knots))))

(defun part-2 ()
  (with-open-file (f (asdf:system-relative-pathname :advent-of-code-2022-in-common-lisp "src/2022-day-9.in"))
    (->> (read-moves f)
      run-moves
      size)))
