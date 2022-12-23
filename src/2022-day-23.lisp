(defpackage 2022-day-23
  (:use :cl :iterate :cl-ppcre :metabang-bind :trivia :trivia.ppcre)
  (:shadowing-import-from :arrow-macros :->>))
(in-package 2022-day-23)

(defun part-1 ()
  (bind ((input (with-open-file (f (asdf:system-relative-pathname :advent-of-code-2022-in-common-lisp "src/2022-day-23.in"))
                  (streams-lines f)))
         (grid (read-grid input)))
    (->> (make-moves 10 'N grid)
      smallest-rectangle-blanks)))

;; Wrong: 2557

(defun make-moves (moves-left direction-bias grid)
  (if (= 0 moves-left)
      grid
      (bind ((new-grid (make-hash-table :test #'equal))
             (new-direction (next-direction direction-bias))
             (proposed-moves (propose-moves grid direction-bias)))
        (let ((moved-any (place-on-grid new-grid proposed-moves)))
          (if (not moved-any)
              moves-left
              (make-moves (1- moves-left)
                          new-direction
                          new-grid))))))

(defun print-grid (grid)
  (bind (((min-x min-y max-x max-y) (grid-bounds grid)))
    (iter
      (for y from min-y to max-y)
      (iter
        (for x from min-x to max-x)
        (if (gethash (cons x y) grid)
            (format t "#")
            (format t ".")))
      (format t "~%"))))

(defun place-on-grid (grid proposals)
  (bind ((taken-counts (make-hash-table :test #'equal))
         (any-moved nil))
    (iter
      (for (start . dest) in proposals)
      (incf (gethash dest taken-counts 0)))
    (iter
      (for (start . dest) in proposals)
      (if (= (gethash dest taken-counts) 1)
          (progn
            (setf (gethash dest grid) t)
            (when (not (equal start dest))
              (setf any-moved t)))
          (setf (gethash start grid) t)))
    any-moved))

(defun propose-moves (grid direction)
  (iter
    (for (pos value) in-hashtable grid)
    (for (x . y) = pos)
    (if (should-move x y grid)
        (collecting (cons pos (new-pos x y grid direction)))
        (collecting (cons pos pos)))))

(defun next-direction (direction)
  (case direction
    (N 'S)
    (S 'W)
    (W 'E)
    (E 'N)))

(defun new-pos (x y grid bias)
  (let ((none-in-top (and (null (gethash (cons x      (1- y)) grid))
                          (null (gethash (cons (1+ x) (1- y)) grid))
                          (null (gethash (cons (1- x) (1- y)) grid))))

        (none-in-bottom (and (null (gethash (cons x      (1+ y)) grid))
                             (null (gethash (cons (1+ x) (1+ y)) grid))
                             (null (gethash (cons (1- x) (1+ y)) grid))))

        (none-in-left (and (null (gethash (cons (1- x) y) grid))
                           (null (gethash (cons (1- x) (1+ y)) grid))
                           (null (gethash (cons (1- x) (1- y)) grid))))

        (none-in-right (and (null (gethash (cons (1+ x) y) grid))
                            (null (gethash (cons (1+ x) (1+ y)) grid))
                            (null (gethash (cons (1+ x) (1- y)) grid)))))
    (case bias
      (N (or (and none-in-top    (cons x      (1- y)))
             (and none-in-bottom (cons x      (1+ y)))
             (and none-in-left   (cons (1- x) y))
             (and none-in-right  (cons (1+ x) y))
             (cons x y)))

      (S (or (and none-in-bottom (cons x      (1+ y)))
             (and none-in-left   (cons (1- x) y))
             (and none-in-right  (cons (1+ x) y))
             (and none-in-top    (cons x      (1- y)))
             (cons x y)))

      (W (or (and none-in-left   (cons (1- x) y))
             (and none-in-right  (cons (1+ x) y))
             (and none-in-top    (cons x      (1- y)))
             (and none-in-bottom (cons x      (1+ y)))
             (cons x y)))

      (E (or (and none-in-right  (cons (1+ x) y))
             (and none-in-top    (cons x      (1- y)))
             (and none-in-bottom (cons x      (1+ y)))
             (and none-in-left   (cons (1- x) y))
             (cons x y))))))

(defun should-move (x y grid)
  (iter
    (for xx from (1- x) to (1+ x))
    (thereis
     (iter
       (for yy from (1- y) to (1+ y))
       (when (and (= xx x)
                  (= yy y))
         (next-iteration))
       (for found = (gethash (cons xx yy) grid))
       (thereis found)))))

(defun read-grid (lines)
  (iter
    (with grid = (make-hash-table :test #'equal))
    (for line in lines)
    (for y from 0)
    (iter
      (for char in-string line)
      (for x from 0)
      (when (eq char #\#)
        (setf (gethash (cons x y) grid) t)))
    (finally (return grid))))

(defun grid-bounds (grid)
  (iter
    (for (key value) in-hashtable grid)
    (for (x . y) = key)
    (minimizing x into min-x)
    (maximizing x into max-x)
    (minimizing y into min-y)
    (maximizing y into max-y)
    (finally
     (return (list min-x min-y max-x max-y)))))

(defun smallest-rectangle-blanks (grid)
  (bind (((min-x min-y max-x max-y) (grid-bounds grid)))
    (iter
      (for x from min-x to max-x)
      (summing
       (iter
         (for y from min-y to max-y)
         (counting (null (gethash (cons x y) grid))))))))

(defun streams-lines (f)
  (do ((line (read-line f nil nil) (read-line f nil nil))
       (lines))
      ((null line) (nreverse lines))
    (push line lines)))

(defun part-2 ()
  (bind ((input (with-open-file (f (asdf:system-relative-pathname :advent-of-code-2022-in-common-lisp "src/2022-day-23.in"))
                  (streams-lines f)))
         (grid (read-grid input)))
    (1+ (- 1000 (make-moves 1000 'N grid)))))
