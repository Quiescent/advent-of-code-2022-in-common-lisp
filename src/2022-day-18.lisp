(defpackage 2022-day-18
  (:use :cl :iterate :cl-ppcre :metabang-bind :trivia :trivia.ppcre)
  (:shadowing-import-from :arrow-macros :->>))
(in-package 2022-day-18)

(defun part-1 ()
  (bind ((cubes (with-open-file (f (asdf:system-relative-pathname :advent-of-code-2022-in-common-lisp "src/2022-day-18.in"))
                  (streams-lines f)))
         (bounds (bounds cubes))
         (grid (apply #'paint-grid cubes (nthcdr 3 bounds))))
    (apply #'surface-points grid (nthcdr 3 bounds))))

(defun surface-points (grid max-x max-y max-z)
  (+ (iter
       (for x from 0 below (+ 3 max-x))
       (summing
        (iter
          (for y from 0 below (+ 3 max-y))
          (summing
           (iter
             (for z from 0 below (+ 3 max-z))
             (for square = (aref grid x y z))
             (for p-square previous square initially 'none)
             (counting (and (not (eq p-square 'none))
                            (or (and square (not p-square))
                                (and (not square) p-square)))))))))
     (iter
       (for z from 0 below (+ 3 max-z))
       (summing
        (iter
          (for y from 0 below (+ 3 max-y))
          (summing
           (iter
             (for x from 0 below (+ 3 max-x))
             (for square = (aref grid x y z))
             (for p-square previous square initially 'none)
             (counting (and (not (eq p-square 'none))
                            (or (and square (not p-square))
                                (and (not square) p-square)))))))))
     (iter
       (for x from 0 below (+ 3 max-x))
       (summing
        (iter
          (for z from 0 below (+ 3 max-z))
          (summing
           (iter
             (for y from 0 below (+ 3 max-y))
             (for square = (aref grid x y z))
             (for p-square previous square initially 'none)
             (counting (and (not (eq p-square 'none))
                            (or (and square (not p-square))
                                (and (not square) p-square)))))))))))

(defun bounds (cubes)
  (iter
    (for (x y z) in cubes)
    (minimizing x into min-x)
    (minimizing y into min-y)
    (minimizing z into min-z)
    (maximizing x into max-x)
    (maximizing y into max-y)
    (maximizing z into max-z)
    (finally
     (return (list min-x min-y min-z max-x max-y max-z)))))

;; Note: some cubes start at 0!
(defun paint-grid (cubes max-x max-y max-z)
  (let ((grid (make-array (list (+ max-x 3) (+ max-y 3) (+ max-z 3))
                          :initial-element nil)))
    (iter
      (for (x y z) in cubes)
      (iter
        (for xx from x below (1+ x))
        (iter
          (for yy from y below (1+ y))
          (iter
            (for zz from z below (1+ z))
            (setf (aref grid (1+ xx) (1+ yy) (1+ zz)) t)))))
    grid))

(defun streams-lines (f)
  (do ((line (read-line f nil nil) (read-line f nil nil))
       (lines))
      ((null line) (nreverse lines))
    (push (->> (split "," line)
            (mapcar #'read-from-string))
          lines)))

;; Wrong 1197
;; Wrong 4601

(defun part-2 ()
  (bind ((cubes (with-open-file (f (asdf:system-relative-pathname :advent-of-code-2022-in-common-lisp "src/2022-day-18.in"))
                  (streams-lines f)))
         (bounds (bounds cubes))
         (grid (apply #'paint-grid cubes (nthcdr 3 bounds))))
    (apply #'reachable-area grid (nthcdr 3 bounds))))

(defvar deltas '((1 0 0) (-1 0 0) (0 1 0) (0 -1 0) (0 0 1) (0 0 -1)))

(defun reachable-area (grid max-x max-y max-z)
  (let ((seen (make-array (list (+ 3 max-x) (+ 3 max-y) (+ 3 max-z))
                          :initial-element nil))
        (total 0))
    (labels ((recur (x y z)
               (progn
                 (when (null (aref grid x y z))
                   (incf total
                         (iter
                           (for (dx dy dz) in deltas)
                           (for xx = (+ x dx))
                           (for yy = (+ y dy))
                           (for zz = (+ z dz))
                           (counting (and (>= xx 0)
                                          (< xx (+ 3 max-x))
                                          (>= yy 0)
                                          (< yy (+ 3 max-y))
                                          (>= zz 0)
                                          (< zz (+ 3 max-z))
                                          (aref grid xx yy zz))))))
                 (iter
                   (for (dx dy dz) in deltas)
                   (for xx = (+ x dx))
                   (for yy = (+ y dy))
                   (for zz = (+ z dz))
                   (when (and (>= xx 0)
                              (< xx (+ 3 max-x))
                              (>= yy 0)
                              (< yy (+ 3 max-y))
                              (>= zz 0)
                              (< zz (+ 3 max-z))
                              (not (aref seen xx yy zz))
                              (not (aref grid xx yy zz)))
                     (setf (aref seen xx yy zz) t)
                     (recur xx yy zz))))))
      (recur 0 0 0)
      total)))

;; Wrong 2619
