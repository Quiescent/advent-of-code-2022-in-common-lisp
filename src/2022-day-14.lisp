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
      (for y from (- min-y 4) below max-y)
      (iter
        (for x from min-x below max-x)
        (if (aref plane x y)
            (format t "#")
            (format t ".")))
      (format t "~%"))))

(defun drop-grain (plane min-x min-y max-x max-y)
  (iter
    (with x = 500)
    (with y = 0)
    (when (>= y max-y)
      (return 'fell))
    (for d = (+ y 1))
    (for l = (- x 1))
    (for r = (+ x 1))
    (for dd = (aref plane x d))
    (for dl = (aref plane l d))
    (for dr = (aref plane r d))
    (if dd
        (if dl
            (if dr
                (progn
                  (setf (aref plane x y) t)
                  (return (cons x y)))
                (progn
                  (incf x)
                  (incf y)))
            (progn
              (decf x)
              (incf y)))
        (incf y))))

(defun simulate-sand (plane min-x min-y max-x max-y)
  (iter
    (with fell-off-bottom = nil)
    (while (not (eq fell-off-bottom 'fell)))
    (setf fell-off-bottom
          (drop-grain plane min-x min-y max-x max-y))
    (counting t)))

(defvar total-grains 0)

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
         (plane (make-array (list (1+ max-x) (1+ max-y)) :initial-element nil))
         (total-grains 0))
    (fill-lines input plane)
    (simulate-with-drawing plane min-x min-y max-x max-y)
    (format t "Grains: ~a~%" total-grains)))

(defun draw-plane (plane win min-x min-y camera-x camera-y)
  (bind (((max-x max-y) (array-dimensions plane))
         (start-y (max 0 (- camera-y 15)))
         (end-y (min (+ camera-y 15) max-y))
         (start-x (max 0 (- camera-x 40)))
         (end-x (min max-x (+ camera-x 40))))
    (iter
      (for y from start-y below end-y)
      (iter
        (for x from start-x below end-x)
        ;; (with-open-file (f (asdf:system-relative-pathname :advent-of-code-2022-in-common-lisp "out")
        ;;                    :if-exists :append
        ;;                    :if-does-not-exist :create
        ;;                    :direction :output)
        ;;   (format f "(list x y): ~a~%" (list (- x start-x)
        ;;                                      (- y start-y))))
        (croatoan:move win (- y start-y) (- x start-x))
        (if (aref plane x y)
            (croatoan:add win #\#)
            (croatoan:add win #\.))))
    (croatoan:refresh win)))

(defun simulate-with-drawing (plane min-x min-y max-x max-y)
  (bind ((grains 0)
         (camera-x 500)
         (camera-y 15))
    (croatoan:with-screen (scr
                           :input-blocking nil
                           :input-echoing nil
                           :cursor-visible nil)
      (croatoan:clear scr)
      (croatoan:bind scr #\q (lambda (win e)
                               (croatoan:exit-event-loop win e)))
      (croatoan:bind scr nil (lambda (win e)
                               (bind ((last-coord (drop-grain plane min-x min-y max-x max-y)))
                                 (when (eq 'fell last-coord)
                                   (setf total-grains grains)
                                   (croatoan:exit-event-loop win e))
                                 (incf grains)
                                 (bind (((last-x . last-y) last-coord))
                                   (when (or (> last-x (+ camera-x 40))
                                             (< last-x (- camera-x 40)))
                                     (setf camera-x (- last-x 40))
                                     (croatoan:clear win))
                                   (when (or (> last-y (+ camera-y 15))
                                             (< last-y (- camera-y 40)))
                                     (setf camera-y (- last-y 15))
                                     (croatoan:clear win)))
                                 (draw-plane plane win min-x min-y camera-x camera-y))))
      (setf (croatoan:frame-rate scr) 30)
      (croatoan:run-event-loop scr))))

(defun drop-grain-floor (plane min-x min-y max-x max-y)
  (iter
    (with x = 500)
    (with y = 0)
    ;; (when (>= y max-y)
    ;;   (return t))
    (for d = (+ y 1))
    (for l = (- x 1))
    (for r = (+ x 1))
    (for dd = (gethash (cons x d) plane))
    (for dl = (gethash (cons l d) plane))
    (for dr = (gethash (cons r d) plane))
    (if dd
        (if dl
            (if dr
                (progn
                  (if (and (= x 500)
                           (= y 0))
                      (return t)
                      (progn
                        (setf (gethash (cons x y) plane) t)
                        (return nil))))
                (progn
                  (incf x)
                  (incf y)))
            (progn
              (decf x)
              (incf y)))
        (incf y))))

(defun print-plane-floor (plane min-x min-y max-x max-y)
  (iter
    (for y from (- min-y 4) below (+ 3 max-y))
    (iter
      (for x from (- min-x 4) below (+ max-x 4))
      (if (gethash (cons x y) plane)
          (format t "#")
          (format t ".")))
    (format t "~%")))

(defun simulate-sand-floor (plane min-x min-y max-x max-y)
  (iter
    (with fell-off-bottom = nil)
    (while (not fell-off-bottom))
    (setf fell-off-bottom
          (drop-grain-floor plane min-x min-y max-x max-y))
    (counting t)))

(defun fill-lines-floor (lines plane)
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
            (setf (gethash (cons xx y) plane) t))
          (iter
            (for yy from (min p-y y) to (max p-y y))
            (setf (gethash (cons x yy) plane) t))))))

(defun fill-bottom-line (plane min-x min-y max-x max-y)
  (iter
    (with y = (+ 2 max-y))
    (for x from (- 500 (* 3 (- max-y min-y))) below (+ 500 (* 3 (- max-y min-y))))
    (setf (gethash (cons x y) plane) t)))

(defun part-2 ()
  (bind ((input (with-open-file (f (asdf:system-relative-pathname :advent-of-code-2022-in-common-lisp "src/2022-day-14.in"))
                 (do ((line (read-line f nil nil) (read-line f nil nil))
                      (lines (list)))
                     ((null line) lines)
                   (push (->> (split " -> " line)
                           (mapcar (lambda (coord) (mapcar #'read-from-string
                                                           (split "," coord) ))))
                         lines))))
         ((min-x min-y max-x max-y) (bounds input))
         (plane (make-hash-table :test #'equal)))
    (fill-lines-floor input plane)
    (fill-bottom-line plane min-x min-y max-x max-y)
    (simulate-sand-floor plane min-x min-y max-x max-y)))
