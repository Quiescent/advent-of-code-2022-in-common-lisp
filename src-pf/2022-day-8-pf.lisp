(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun shadowing-import-from-fset ()
    `(:shadowing-import-from
      :fset
      ,@(let ((symbols nil))
          (do-external-symbols (symbol (find-package 'fset) symbols)
            (push symbol symbols))))))

(defpackage 2022-day-8-pf
  (:use :cl :iterate :cl-ppcre :metabang-bind :trivia :trivia.ppcre)
  #.(shadowing-import-from-fset)
  (:shadowing-import-from :arrow-macros :->>)
  (:shadowing-import-from :arrow-macros :->))
(in-package 2022-day-8-pf)

(defun all-lines (f)
  (labels ((recur ()
             (bind ((line (->> (read-line f nil nil)
                            (cl:map 'list #'digit-char-p))))
               (unless (null line)
                 (cons line (recur))))))
    (recur)))

(defun read-grid (f)
  (bind ((lines (all-lines f))
         (x-dim (length (car lines)))
         (y-dim (length lines)))
    (make-array (list y-dim x-dim) :initial-contents lines)))

(defun visible-from-side (grid)
  (bind (((y-dim x-dim) (array-dimensions grid)))
    (labels ((recur-x (acc highest inc x y)
               (if (or (< x 0)
                       (>= x x-dim))
                   acc
                   (bind ((tree (aref grid y x)))
                     (recur-x (if (< highest tree)
                                 (with acc (cons x y))
                                 acc)
                             (max tree highest)
                             inc
                             (funcall inc x)
                             y))))
             (recur-y (acc highest inc x y)
               (if (or (< y 0)
                       (>= y y-dim))
                   acc
                   (bind ((tree (aref grid y x)))
                     (recur-y (if (< highest tree)
                                 (with acc (cons x y))
                                 acc)
                             (max tree highest)
                             inc
                             x
                             (funcall inc y)))))
             (for-all-ys (f acc)
               (labels ((recur (acc y)
                          (if (>= y y-dim)
                              acc
                              (recur (funcall f acc y)
                                (1+ y)))))
                 (recur acc 0)))
             (for-all-xs (f acc)
               (labels ((recur (acc x)
                          (if (>= x x-dim)
                              acc
                              (recur (funcall f acc x)
                                (1+ x)))))
                 (recur acc 0))))
      (->> (for-all-ys (lambda (acc y)
                         (recur-x acc most-negative-fixnum #'1+ 0 y))
                       (set))
        (for-all-ys (lambda (acc y)
                      (recur-x acc most-negative-fixnum #'1- (1- x-dim) y)))
        (for-all-xs (lambda (acc x)
                      (recur-y acc most-negative-fixnum #'1+ x 0)))
        (for-all-xs (lambda (acc x)
                      (recur-y acc most-negative-fixnum #'1- x (1- y-dim))))
        (size)))))

(defun part-1 ()
  (with-open-file (f (asdf:system-relative-pathname :advent-of-code-2022-in-common-lisp "src/2022-day-8.in"))
    (bind ((grid (read-grid f)))
      (visible-from-side grid))))

(defun scenic-score (grid x y)
  (bind (((y-dim x-dim) (array-dimensions grid))
         (tree-height (aref grid y x)))
    (labels ((recur-x (acc inc x y)
               (cond
                 ((or (< x 0) (>= x x-dim))        acc)
                 ((<= tree-height (aref grid y x)) (1+ acc))
                 (t (recur-x (1+ acc)
                            inc
                            (funcall inc x)
                            y))))
             (recur-y (acc inc x y)
               (cond
                 ((or (< y 0) (>= y y-dim)) acc)
                 ((<= tree-height (aref grid y x)) (1+ acc))
                 (t (recur-y (1+ acc)
                            inc
                            x
                            (funcall inc y))))))
      (* (recur-x 0 #'1+ (1+ x) y)
         (recur-x 0 #'1- (1- x) y)
         (recur-y 0 #'1+ x (1+ y))
         (recur-y 0 #'1- x (1- y))))))

(defun most-scenic-score (grid)
  (bind (((x-dim y-dim) (array-dimensions grid)))
    (labels ((recur (acc x y)
               (cond
                 ((>= x x-dim) (recur acc 0 (1+ y)))
                 ((>= y y-dim) acc)
                 (t (recur (max acc (scenic-score grid x y))
                           (1+ x) y)))))
      (recur most-negative-fixnum 0 0))))

(defun part-2 ()
  (with-open-file (f (asdf:system-relative-pathname :advent-of-code-2022-in-common-lisp "src/2022-day-8.in"))
    (bind ((Grid (read-grid f)))
      (most-scenic-score grid))))
