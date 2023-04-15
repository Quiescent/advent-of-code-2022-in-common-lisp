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
