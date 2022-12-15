(defpackage 2022-day-15
  (:use :cl :iterate :cl-ppcre :metabang-bind :trivia :trivia.ppcre)
  (:shadowing-import-from :arrow-macros :->>))
(in-package 2022-day-15)

(defun part-1 ()
  (let ((input (with-open-file (f (asdf:system-relative-pathname :advent-of-code-2022-in-common-lisp "src/2022-day-15.in"))
                 (do ((line (read-line f nil nil) (read-line f nil nil))
                      (lines))
                     ((null line) lines)
                   (push (match line
                           ((ppcre "Sensor at x=(-?\\d+), y=(-?\\d+): closest beacon is at x=(-?\\d+), y=(-?\\d+)"
                                   (read sx)
                                   (read sy)
                                   (read bx)
                                   (read by))
                            (list sx sy bx by)))
                         lines)))))
    (count-positions input)))

(defun all-positions (sensor-readings)
  (iter
    (with positions = (make-hash-table :test #'equal))
    (for (sx sy bx by) in sensor-readings)
    (setf (gethash (cons sx sy) positions) t
          (gethash (cons bx by) positions) t)
    (finally (return positions))))

(defun count-positions (sensor-readings)
  (iter
    (with all-positions = (all-positions sensor-readings))
    (with coords = (make-hash-table :test #'equal))
    (with grid = (make-hash-table :test #'equal))
    ;; (with y = 2000000)
    (with y = 2000000)
    (for (sx sy bx by) in sensor-readings)
    (for distance = (+ (abs (- sx bx))
                       (abs (- sy by))))
    ;; (format t "distance: ~a~%" distance)
    ;; (format t "(list sx sy): ~a~%" (list sx sy))
    (iter
      (for x from (- sx distance distance) to (+ sx distance distance))
      (when (and (<= (+ (abs (- sx x))
                        (abs (- sy y)))
                     distance)
                 (not (gethash (cons x y) all-positions)))
        (setf (gethash (cons x y) coords) t)))
    (finally
     (return
       (hash-table-count coords)))))

(defun part-2 ()
  (let ((input (with-open-file (f (asdf:system-relative-pathname :advent-of-code-2022-in-common-lisp "src/2022-day-15.in"))
                 (do ((line (read-line f nil nil) (read-line f nil nil))
                      (lines))
                     ((null line) lines)
                   (push (match line
                           ((ppcre "Sensor at x=(-?\\d+), y=(-?\\d+): closest beacon is at x=(-?\\d+), y=(-?\\d+)"
                                   (read sx)
                                   (read sy)
                                   (read bx)
                                   (read by))
                            (list sx sy bx by)))
                         lines)))))
    (beacon-position input)))

(defun beacon-position (sensor-readings)
  (iter outer
    (with positions = (all-positions sensor-readings))
    (for y from 0 to 4000000)
    (iter inner
      (for x from 0 to 4000000)
      (when (iter
              (for (sx sy bx by) in sensor-readings)
              (for range = (+ (abs (- sx bx))
                              (abs (- sy by))))
              (for distance = (+ (abs (- sx x))
                                 (abs (- sy y))))
              (when (<= distance range)
                (incf x (1- (- (1+ (* 2 range))
                               (* 2 (abs (- sy y)))
                               (- range distance))))
                (return nil))
              (finally (return t)))
        (return-from outer (+ (* x 4000000) y))))))
