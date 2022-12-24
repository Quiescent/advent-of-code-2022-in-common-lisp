(defpackage 2022-day-24
  (:use :cl :iterate :cl-ppcre :metabang-bind :trivia :trivia.ppcre)
  (:shadowing-import-from :arrow-macros :->>))
(in-package 2022-day-24)

(defun part-1 ()
  (bind ((lines (with-open-file (f (asdf:system-relative-pathname :advent-of-code-2022-in-common-lisp "src/2022-day-24.in"))
                  (streams-lines f)))
         ((start end x-dim y-dim blizzards) (read-map lines)))
    (shortest-path x-dim y-dim start end blizzards)
    ;; (tick-blizzards x-dim y-dim blizzards)
    ))

;; Wrong: 145
;; Wrong: 309

(defun distance (pos-1 pos-2)
  (bind ((x (- pos-1 pos-2)))
    (+ (abs (realpart x))
       (abs (imagpart x)))))

(defun shortest-path (x-dim y-dim start end blizzards)
  (bind ((blizzard-cache (make-hash-table))
         (cache          (make-hash-table :test #'equal))
         (best           most-positive-fixnum))
    (setf (gethash 0 blizzard-cache) blizzards)
    (labels ((recur (pos time)
               (or #1=(gethash (cons pos time) cache)
                   (setf #1#
                         (cond
                           ((>= time best) best)
                           ((>= (distance end pos)
                                (- best time))
                            best)
                           ((= pos end) (setf best time))
                           (t (bind ((current-blizzards (gethash time
                                                                 blizzard-cache))
                                     (new-blizzards
                                      (or #2=(gethash (1+ time) blizzard-cache)
                                          (setf #2# (tick-blizzards x-dim
                                                                    y-dim
                                                                    current-blizzards))))
                                     (new-positions (find-spots x-dim
                                                                y-dim
                                                                start
                                                                end
                                                                pos
                                                                new-blizzards)))
                                (or (iter
                                      (for position in new-positions)
                                      (when position
                                        (minimizing (recur position (1+ time)))))
                                    best))))))))
      (recur start 0))))

(defun find-spots (x-dim y-dim start end position blizzards)
  (bind ((left  (1- position))
         (right (1+ position))
         (up    (- position #c(0 1)))
         (down  (+ position #c(0 1))))
    (list
     ;; Down
     (when (or (= down end)
               (and (< (imagpart down) y-dim)
                    (null (gethash down blizzards))))
       down)
     ;; Right
     (when (and (not (= position start))
                (< (realpart right) x-dim)
                (null (gethash right blizzards)))
       right)
     ;; Stay at position
     (when (null (gethash position blizzards))
       position)
     ;; Up
     ;; Assume never go back to start!
     (when (and (not (= position start))
                (>= (imagpart up) 0)
                (null (gethash up blizzards)))
       up)
     ;; Left
     (when (and (not (= position start))
                (>= (realpart left) 0)
                (null (gethash left blizzards)))
       left))))

(defun read-map (lines)
  (iter
    (with start = nil)
    (with end = nil)
    (with y-dim = (- (length lines) 2))
    (with x-dim = (- (->> (car lines)
                       length)
                     2))
    (with blizzards = (make-hash-table))
    (for y from 0)
    (for line in lines)
    (iter
      (for char in-string line)
      (for x from 0)
      (when (and (= y (1- (length lines)))
                 (eq char #\.))
        (setf end (complex (1- x) (1- y))))
      (when (and (= y 0)
                 (eq char #\.))
        (setf start (complex (1- x) (1- y))))
      (when (and (not (eq char #\.))
                 (not (eq char #\#)))
        (push (cond
                ((eq char #\>) #c(1 0))
                ((eq char #\<) #c(-1 0))
                ((eq char #\v) #c(0 1))
                ((eq char #\^) #c(0 -1)))
              (gethash (complex (1- x)
                                (1- y))
                       blizzards))))
    (finally
     (return (list start end x-dim y-dim blizzards)))))

(defun tick-blizzards (x-dim y-dim blizzards)
  (iter
    (with next-blizzards = (make-hash-table))
    (for (position directions) in-hashtable blizzards)
    (iter
      (for direction in directions)
      (for new-position = (+ position direction))
      (for final-position =
           (cond
             ;; Left
             ((= (realpart direction) -1) (if (< (realpart new-position) 0)
                                              (complex (1- x-dim)
                                                       (imagpart new-position))
                                              new-position))
             ;; Right
             ((= (realpart direction) 1)  (if (>= (realpart new-position) x-dim)
                                              (complex 0
                                                       (imagpart new-position))
                                              new-position))
             ;; Down
             ((= (imagpart direction) 1)  (if (>= (imagpart new-position) y-dim)
                                              (complex (realpart new-position)
                                                       0)
                                              new-position))
             ;; Up
             ((= (imagpart direction) -1) (if (< (imagpart new-position) 0)
                                              (complex (realpart new-position)
                                                       (1- y-dim))
                                              new-position))))
      (push direction (gethash final-position next-blizzards)))
    (finally (return next-blizzards))))

(defun streams-lines (f)
  (do ((line (read-line f nil nil) (read-line f nil nil))
       (lines))
      ((null line) (nreverse lines))
    (push line lines)))

(defun part-2 ()
  (bind ((lines (with-open-file (f (asdf:system-relative-pathname :advent-of-code-2022-in-common-lisp "src/2022-day-24.in"))
                  (streams-lines f)))
         ((start end x-dim y-dim blizzards) (read-map lines)))
    (shortest-path-there-and-back x-dim y-dim start end blizzards)
    ;; (tick-blizzards x-dim y-dim blizzards)
    ))

;; Wrong: 1114

(defun shortest-path-there-and-back (x-dim y-dim start end blizzards)
  (bind ((blizzard-cache (make-hash-table))
         (cache          (make-hash-table :test #'equal))
         (best           most-positive-fixnum))
    (setf (gethash 0 blizzard-cache) blizzards)
    (labels ((recur-there (pos time cum-time)
               (or #1=(gethash (cons pos time) cache)
                   (setf #1#
                         (cond
                           ((>= time best) best)
                           ((>= (distance end pos)
                                (- best time))
                            best)
                           ((= pos end) (setf best time))
                           (t (bind ((current-blizzards (gethash cum-time
                                                                 blizzard-cache))
                                     (new-blizzards
                                      (or #2=(gethash (1+ cum-time)
                                                      blizzard-cache)
                                          (setf #2# (tick-blizzards x-dim
                                                                    y-dim
                                                                    current-blizzards))))
                                     (new-positions (find-spots x-dim
                                                                y-dim
                                                                start
                                                                end
                                                                pos
                                                                new-blizzards)))
                                (or (iter
                                      (for position in new-positions)
                                      (when position
                                        (minimizing (recur-there position
                                                                 (1+ time)
                                                                 (1+ cum-time)))))
                                    best)))))))
             (recur-back (pos time cum-time)
               (or #3=(gethash (cons pos time) cache)
                   (setf #3#
                         (cond
                           ((>= time best) best)
                           ((>= (distance start pos)
                                (- best time))
                            best)
                           ((= pos start) (setf best time))
                           (t (bind ((current-blizzards (gethash cum-time
                                                                 blizzard-cache))
                                     (new-blizzards
                                      (or #4=(gethash (1+ cum-time)
                                                      blizzard-cache)
                                          (setf #4# (tick-blizzards x-dim
                                                                    y-dim
                                                                    current-blizzards))))
                                     (new-positions (find-spots-back x-dim
                                                                     y-dim
                                                                     start
                                                                     end
                                                                     pos
                                                                     new-blizzards)))
                                (or (iter
                                      (for position in new-positions)
                                      (when position
                                        (minimizing (recur-back position
                                                                (1+ time)
                                                                (1+ cum-time)))))
                                    best))))))))
      (bind ((there (recur-there start 0 0)))
        (format t "there: ~a~%" there)
        (setf cache (make-hash-table :test #'equal))
        (setf best most-positive-fixnum)
        (bind ((back (recur-back end 0 there)))
          (format t "back: ~a~%" back)
          (setf cache (make-hash-table :test #'equal))
          (setf best most-positive-fixnum)
          (bind ((there-again (recur-there start 0 (+ there back))))
            (format t "there-again: ~a~%" there-again)
            (+ there-again
               there
               back)))))))

(defun find-spots-back (x-dim y-dim start end position blizzards)
  (bind ((left  (1- position))
         (right (1+ position))
         (up    (- position #c(0 1)))
         (down  (+ position #c(0 1))))
    (list
     ;; Up
     ;; Assume never go back to start!
     (when (or (= up start)
               (and (>= (imagpart up) 0)
                    (null (gethash up blizzards))))
       up)
     ;; Left
     (when (and (not (= position end))
                (>= (realpart left) 0)
                (null (gethash left blizzards)))
       left)
     ;; Stay at position
     (when (null (gethash position blizzards))
       position)
     ;; Down
     (when (and (not (= position end))
                (< (imagpart down) y-dim)
                (null (gethash down blizzards)))
       down)
     ;; Right
     (when (and (not (= position end))
                (< (realpart right) x-dim)
                (null (gethash right blizzards)))
       right))))
