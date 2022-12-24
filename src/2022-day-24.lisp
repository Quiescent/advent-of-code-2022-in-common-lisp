(defpackage 2022-day-24
  (:use :cl :iterate :cl-ppcre :metabang-bind :trivia :trivia.ppcre)
  (:shadowing-import-from :arrow-macros :->>))
(in-package 2022-day-24)

(defun part-1 ()
  (bind ((((x . y) blizzards start end) (fresh-grid)))
    (shortest-path x y blizzards start end)))

;; Wrong: 549
;; Wrong: 174

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

(defun collides (position blizzards)
  (iter
    (for (blizzard-position . direction) in blizzards)
    (thereis (= position blizzard-position))))

(defun find-empty-spots (start end x-dim y-dim position blizzards)
  (bind ((left  (1- position))
         (right (1+ position))
         (down  (+ position #c(0 1)))
         (up    (- position #c(0 1))))
    (list
     ;; Down
     (when (or (= down end)
               (and (< (imagpart down) y-dim)
                    (not (find down blizzards :key #'car))))
       down)
     ;; Right
     (when (and (not (= start position))
                (< (realpart right) x-dim)
                (not (find right blizzards :key #'car)))
       right)
     ;; Left
     (when (and (not (= start position))
                (>= (realpart left) 0)
                (not (find left blizzards :key #'car)))
       left)
     ;; Up
     (when (or (not (= start position))
               (= up start)
               (and (>= (imagpart up) 0)
                    (not (find up blizzards :key #'car))))
       up)
     ;; Stay put
     (when (not (find position blizzards))
       position))))

(defun distance (pos-1 pos-2)
  (bind ((x (- pos-1 pos-2)))
    (+ (abs (realpart x))
       (abs (imagpart x)))))

(defun shortest-path (x-dim y-dim blizzards start end)
  (let ((best-found         nil)
        (blizzards-per-tick (make-hash-table))
        (cache              (make-hash-table :test #'equal)))
    (labels ((recur (pos current-blizzards time)
               (progn
                 (format t "(list pos time): ~a~%" (list pos time))
                 (or #2=(gethash (cons pos time) cache)
                     (setf #2#
                           (cond
                             ((and best-found
                                   (> (distance pos end)
                                      (- best-found time)))
                              best-found)
                             ;; ((> time 10) most-positive-fixnum)
                             ((and best-found (>= time best-found)) best-found)
                             ((= pos end) (progn
                                            (setf best-found time)
                                            (format t "best-found: ~a~%" best-found)
                                            time))
                             (t (bind ((new-blizzards
                                        (or #1=(gethash time blizzards-per-tick)
                                            (setf #1# (tick-blizzards x-dim
                                                                      y-dim
                                                                      current-blizzards))))
                                       (empty-spots (find-empty-spots start
                                                                      end
                                                                      x-dim
                                                                      y-dim
                                                                      pos
                                                                      new-blizzards)))
                                  (iter
                                    (for new-position in empty-spots)
                                    (when (not (null new-position))
                                      (minimizing (recur new-position
                                                         new-blizzards
                                                         (1+ time)))))))))))))
      (recur start blizzards 0))))

(defun fresh-grid ()
  (iter outer
    (with lines = (with-open-file (f (asdf:system-relative-pathname :advent-of-code-2022-in-common-lisp "src/2022-day-24.in"))
                    (streams-lines f)))
    (with y-dim = (- (length lines) 2))
    (with x-dim = (- (->> (car lines)
                       length)
                     2))
    (with start = nil)
    (with end = nil)
    (for line in lines)
    (for y from 0)
    (iter
      (for char in-string line)
      (for x from 0)
      (when (and (= y 0)
                 (eq char #\.))
        (setf start (complex (1- x) (1- y))))
      (when (and (= y (1+ y-dim))
                 (eq char #\.))
        (setf end (complex (1- x) (1- y))))
      (when (and (not (eq char #\#))
                 (not (eq char #\.)))
        (in outer (collecting (cons (complex (1- x) (1- y))
                                    (cond
                                      ((eq char #\>) #c(1 0))
                                      ((eq char #\<) #c(-1 0))
                                      ((eq char #\^) #c(0 -1))
                                      ((eq char #\v) #c(0 1))))
                              into blizzards))))
    (finally (return-from outer
               (list (cons x-dim y-dim)
                     blizzards
                     start
                     end)))))

(defun streams-lines (f)
  (do ((line (read-line f nil nil) (read-line f nil nil))
       (lines))
      ((null line) (nreverse lines))
    (push line lines)))

(defun part-2 ()
  (let ((input (with-open-file (f (asdf:system-relative-pathname :advent-of-code-2022-in-common-lisp "src/2022-day-24.in"))
                 )))
    ))
