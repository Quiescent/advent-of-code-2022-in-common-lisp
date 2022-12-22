(defpackage 2022-day-22
  (:use :cl :iterate :cl-ppcre :metabang-bind :trivia :trivia.ppcre)
  (:shadowing-import-from :arrow-macros :->>))
(in-package 2022-day-22)

(defun part-1 ()
  (bind ((input (with-open-file (f (asdf:system-relative-pathname :advent-of-code-2022-in-common-lisp "src/2022-day-22.in"))
                  (streams-lines f)))
         ((map-lines . instructions) (split-lines input))
         ((max-x max-y grid) (grid-and-bounds map-lines))
         ((start-x . start-y) (starting-coord grid max-x)))
    (format t "(list start-x start-y): ~a~%" (list start-x start-y))
    (follow-instructions grid start-x start-y max-x max-y instructions)))

(defun starting-coord (grid max-x)
  (iter
    (for x from 1 to max-x)
    (with y = 1)
    (when (eq (gethash (cons x y) grid) 'open)
     (finding (cons x y) minimizing x))))

(defun follow-instructions (grid start-x start-y max-x max-y instructions)
  (iter
    (with i = 0)
    (with position = (complex start-x start-y))
    (with direction = #c(1 0))
    (while (< i (length instructions)))
    (format t "i: ~a~%" i)
    (match (subseq instructions i)
      ((ppcre "(\\d+)(L|R)"
              (read amount)
              (read turn))
       (format t "(list amount turn): ~a~%" (list amount turn))
       (incf i (length (format nil "~a~a" amount turn)))
       (iter
         (for i from 0 below amount)
         (for new-position = (+ position direction))
         (for square = (gethash (cons (realpart new-position)
                                      (imagpart new-position))
                                grid))
         ;; (format t "square: ~a~%" square)
         (case square
           (wall (finish))
           (open (setf position new-position)))
         (when (null square)
           (iter
             (with back-pos = (- position direction))
             (for square = (gethash (cons (realpart back-pos)
                                          (imagpart back-pos))
                                    grid))
             (for p-square previous square)
             (for current-back-pos = back-pos)
             (for p-back-pos previous current-back-pos)
             (while (not (null square)))
             (decf back-pos direction)
             (finally
              (when (not (eq 'wall p-square))
                ;; (format t "found p-back-pos: ~a~%" p-back-pos)
                (setf position p-back-pos)))))
         ;; (format t "position: ~a~%" position)
         )
       (case turn
         (R (setf direction (* direction #c(0 1))))
         (L (setf direction (* direction #c(0 -1))))))
      ((ppcre "(\\d+)"
              (read amount))
       (format t "(list amount turn): ~a~%" (list amount))
       (incf i (length (format nil "~a" amount)))
       (iter
         (for i from 0 below amount)
         (for new-position = (+ position direction))
         (for square = (gethash (cons (realpart new-position)
                                      (imagpart new-position))
                                grid))
         ;; (format t "square: ~a~%" square)
         (case square
           (wall (finish))
           (open (setf position new-position)))
         (when (null square)
           (iter
             (with back-pos = (- position direction))
             (for square = (gethash (cons (realpart back-pos)
                                          (imagpart back-pos))
                                    grid))
             (for p-square previous square)
             (for current-back-pos = back-pos)
             (for p-back-pos previous current-back-pos)
             (while (not (null square)))
             (decf back-pos direction)
             (finally
              (when (not (eq 'wall p-square))
                ;; (format t "found p-back-pos: ~a~%" p-back-pos)
                (setf position p-back-pos)))))
         ;; (format t "position: ~a~%" position)
         )))
    (format t "(list position direction): ~a~%" (list position direction))
    (finally
     (return
       (progn

        (+ (* 1000 (imagpart position))
           (* 4 (realpart position))
           (or (cond
                   ((equal direction #c(1 0)) 0)
                   ((equal direction #c(0 1)) 1)
                   ((equal direction #c(-1 0)) 2)
                   ((equal direction #c(0 -1)) 3)) 0)))))))

;; Wrong: 144240
;; Wrong: 144243

(defun split-lines (lines)
  (iter
    (with in-map = t)
    (with instructions = nil)
    (for line in lines)
    (when (equal line "")
      (setf in-map nil))
    (if in-map
        (collecting line into map-lines)
        (when (not (equal line ""))
          (setf instructions line)))
    (finally
     (return (cons map-lines instructions)))))

(defun grid-and-bounds (lines)
  (iter
    (with grid = (make-hash-table :test #'equal))
    (for line in lines)
    (for y from 1)
    (iter
      (for x from 1)
      (while (< (1- x) (length line)))
      (for square = (aref line (1- x)))
      (for is-wall = (eq square #\#))
      (for is-open = (eq square #\.))
      (when is-wall
        (setf (gethash (cons x y) grid) 'wall))
      (when is-open
        (setf (gethash (cons x y) grid) 'open)))
    (finally
     (return
       (list (length (car lines))
             (length lines)
             grid)))))

(defun streams-lines (f)
  (do ((line (read-line f nil nil) (read-line f nil nil))
       (lines))
      ((null line) (nreverse lines))
    (push line lines)))

(defun part-2 ()
  (bind ((input (with-open-file (f (asdf:system-relative-pathname :advent-of-code-2022-in-common-lisp "src/2022-day-22.in"))
                  (streams-lines f)))
         ((faces . instructions) (faces-and-instructions input)))
    (follow-instructions faces instructions)))

;; Wrong: 108310
;; Wrong: 12210
;; Wrong: 12410
;; Wrong: 10449
;; Wrong: 8502

#+(not example)
(defun faces-and-instructions (lines)
  (bind ((face-length (iter
                        (for line in lines)
                        (for stripped = (cl-ppcre:regex-replace-all " " line ""))
                        (while (not (equal line "")))
                        (minimizing (length stripped)))))
    (format t "face-length: ~a~%" face-length)
    (labels ((face-from (x-offset y-offset)
               (iter
                 (with face = (make-array (list face-length face-length)
                                          :initial-element nil))
                 (for y from 0 below face-length)
                 (for line in (subseq lines y-offset))
                 (iter
                   (for square
                        in-string (subseq (cl-ppcre:regex-replace-all " "
                                                                      line
                                                                      "")
                                          x-offset))
                   (for x from 0 below face-length)
                   (when (eq #\# square)
                     (setf (aref face y x) t)))
                 (finally (return face)))))
      (cons (list (face-from 0                 0)
                  (face-from face-length       0)

                  (face-from 0                 face-length)
                  
                  (face-from 0                 (* 2 face-length))
                  (face-from face-length       (* 2 face-length))

                  (face-from 0                 (* 3 face-length)))
            (nth (1+ (* 4 face-length)) lines)))))

;; Example
#+example
(defun faces-and-instructions (lines)
  (bind ((face-length (iter
                        (for line in lines)
                        (for stripped = (cl-ppcre:regex-replace-all " " line ""))
                        (while (not (equal line "")))
                        (minimizing (length stripped)))))
    (format t "face-length: ~a~%" face-length)
    (labels ((face-from (x-offset y-offset)
               (iter
                 (with face = (make-array (list face-length face-length)
                                          :initial-element nil))
                 (for y from 0 below face-length)
                 (for line in (subseq lines y-offset))
                 (iter
                   (for square
                        in-string (subseq (cl-ppcre:regex-replace-all " "
                                                                      line
                                                                      "")
                                          x-offset))
                   (for x from 0 below face-length)
                   (when (eq #\# square)
                     (setf (aref face y x) t)))
                 (finally (return face)))))
      (cons (list (face-from 0                 0)

                  (face-from 0                 face-length)
                  (face-from face-length       face-length)
                  (face-from (* 2 face-length) face-length)

                  (face-from 0                 (* 2 face-length))
                  (face-from face-length       (* 2 face-length)))
            (print (car (last lines)))))))

(defun follow-instructions (faces instructions)
  (iter
    (with position = #c(0 0))
    (with direction = #c(1 0))
    (with face = 0)
    (with i = 0)
    (while (< i (length instructions)))
    (match (subseq instructions i)
      ((ppcre "^(\\d+)"
              amount-str)
       (incf i (length amount-str))
       (format t "position: ~a~%" position)
       (format t "amount: ~a, direction: ~a~%" amount-str direction)
       (bind ((amount (read-from-string amount-str))
              ((new-face new-direction new-position) (move faces
                                                           face
                                                           amount
                                                           position
                                                           direction)))
         (setf face      new-face
               direction new-direction
               position  new-position)
         (format t "(list face position direction): ~a~%" (list face position direction))))
      ((ppcre "^([LR])"
              (read turn))
       (format t "turn: ~a~%" turn)
       (incf i)
       (case turn
         (L (setf direction (* direction #c(0 -1))))
         (R (setf direction (* direction #c(0 1)))))
       (format t "direction: ~a~%" direction)))
    (finally
     (bind (((final-x . final-y) (to-absolute-coord (array-dimension (car faces)
                                                                     0)
                                                    face
                                                    (realpart position)
                                                    (imagpart position))))
       (format t "(list final-x final-y): ~a~%" (list final-x final-y))
       (return (+ (* 1000 (1+ final-y))
                  (* 4 (1+ final-x))
                  (cond
                    ((equal direction #c(1 0)) 0)
                    ((equal direction #c(0 1)) 1)
                    ((equal direction #c(-1 0)) 2)
                    ((equal direction #c(0 -1)) 3))))))))

#+(not example)
(defun to-absolute-coord (face-length face x y)
  (case face
    (0 (cons (+ face-length x)       y))
    (1 (cons (+ (* 2 face-length) x) y))

    (2 (cons (+ face-length x)       (+ y face-length)))

    (3 (cons x                       (+ y (* 2 face-length))))
    (4 (cons (+ face-length x)       (+ y (* 2 face-length))))

    (5 (cons x                       (+ y (* 3 face-length))))))

;; Example
#+example
(defun to-absolute-coord (face-length face x y)
  (case face
    (0 (cons (+ (* 2 face-length) x) y))
    (1 (cons x                       (+ y face-length)))
    (2 (cons (+ x face-length)       (+ y face-length)))
    (3 (cons (+ x (* 2 face-length)) (+ y face-length)))
    (4 (cons (+ x (* 2 face-length)) (+ y (* 2 face-length))))
    (5 (cons (+ x (* 3 face-length)) (+ y (* 2 face-length))))))

#+(not example)
(defun move (faces face amount position direction)
  (if (<= amount 0)
      (list face direction position)
      (bind ((face-length (array-dimension (car faces) 0))
             (x (realpart position))
             (y (imagpart position))
             (new-position (+ position direction))
             (new-x (realpart new-position))
             (new-y (imagpart new-position))
             ((new-face new-direction new-face-x new-face-y)
              (cond
                ;; Right
                ((>= new-x face-length)
                 (case face
                   (0 (list 1 #c(1 0)  0                   y))
                   (1 (list 4 #c(-1 0) (1- face-length )   (- face-length y 1)))
                   (2 (list 1 #c(0 -1) y                   (1- face-length)))
                   (3 (list 4 #c(1 0)  0                   y))
                   (4 (list 1 #c(-1 0) (1- face-length)    (- face-length y 1)))
                   (5 (list 4 #c(0 -1) y                   (1- face-length)))))

                ;; Left
                ((< new-x 0)
                 (case face
                   (0 (list 3 #c(1 0)  0                   (- face-length y 1)))
                   (1 (list 0 #c(-1 0) (1- face-length)    y))
                   (2 (list 3 #c(0 1)  y                   0))
                   (3 (list 0 #c(1 0)  0                   (- face-length y 1)))
                   (4 (list 3 #c(-1 0) (1- face-length)    y))
                   (5 (list 0 #c(0 1)  y                   0))))

                ;; Down
                ((>= new-y face-length)
                 (case face
                   (0 (list 2 #c(0 1)  x                   0))
                   (1 (list 2 #c(-1 0) (1- face-length)    x))
                   (2 (list 4 #c(0 1)  x                   0))
                   (3 (list 5 #c(0 1)  x                   0))
                   (4 (list 5 #c(-1 0) (1- face-length)    x))
                   (5 (list 1 #c(0 1)  x                   0))))

                ;; Up
                ((< new-y 0)
                 (case face
                   (0 (list 5 #c(1 0)  0                   x))
                   (1 (list 5 #c(0 -1) x                   (1- face-length)))
                   (2 (list 0 #c(0 -1) x                   (1- face-length)))
                   (3 (list 2 #c(1 0)  0                   x))
                   (4 (list 2 #c(0 -1) x                   (1- face-length)))
                   (5 (list 3 #c(0 -1) x                   (1- face-length)))))

                ;; Same face
                (t (list face direction new-x new-y))))
             (new-square (aref (nth new-face faces) new-face-y new-face-x)))
        (format t "(list new-face new-face-x new-face-y): ~a~%" (list new-face new-face-x new-face-y))
        (format t "new-square: ~a~%" new-square)
        (if new-square
            (list face direction position)
            (move faces
                  new-face
                  (1- amount)
                  (complex new-face-x new-face-y)
                  new-direction)))))
;; Example
#+example
(defun move (faces face amount position direction)
  (if (<= amount 0)
      (list face direction position)
      (bind ((face-length (array-dimension (car faces) 0))
             (x (realpart position))
             (y (imagpart position))
             (new-position (+ position direction))
             (new-x (realpart new-position))
             (new-y (imagpart new-position))
             ((new-face new-direction new-face-x new-face-y)
              (cond
                ;; Right
                ((>= new-x face-length)
                 (case face
                   (0 (list 5 #c(-1 0) (1- face-length)    (- face-length y 1)))
                   (1 (list 2 #c(1 0)  0                   y))
                   (2 (list 3 #c(1 0)  0                   y))
                   (3 (list 5 #c(0 1)  (- face-length y 1) 0))
                   (4 (list 5 #c(1 0)  0                   y))
                   (5 (list 0 #c(-1 0) (1- face-length)    (- face-length y 1)))))

                ;; Left
                ((< new-x 0)
                 (case face
                   (0 (list 2 #c(0 1)  y                   0))
                   (1 (list 5 #c(0 1)  (- face-length y 1) (1- face-length)))
                   (2 (list 1 #c(-1 0) (1- face-length)    y))
                   (3 (list 2 #c(-1 0) (1- face-length)    y))
                   (4 (list 2 #c(0 1)  (- face-length y 1) (1- face-length)))
                   (5 (list 4 #c(-1 0) (1- face-length)    y))))

                ;; Down
                ((>= new-y face-length)
                 (case face
                   (0 (list 3 #c(0 1)  x                   0))
                   (1 (list 4 #c(0 -1) (- face-length x 1) (1- face-length)))
                   (2 (list 4 #c(1 0)  0                   (- face-length x 1)))
                   (3 (list 4 #c(0 1)  x                   0))
                   (4 (list 1 #c(0 -1) (- face-length x 1) (1- face-length)))
                   (5 (list 1 #c(1 0)  0                   (- face-length x 1)))))

                ;; Up
                ((< new-y 0)
                 (case face
                   (0 (list 1 #c(0 1)  (- face-length x 1) 0))
                   (1 (list 0 #c(0 1)  (- face-length x 1) 0))
                   (2 (list 0 #c(1 0)  0                   x))
                   (3 (list 0 #c(0 -1) x                   (1- face-length)))
                   (4 (list 3 #c(0 -1) x                   (1- face-length)))
                   (5 (list 3 #c(-1 0) (1- face-length)    (- face-length x 1)))))

                ;; Same face
                (t (list face direction new-x new-y))))
             (new-square (aref (nth new-face faces) new-face-y new-face-x)))
        (if new-square
            (list face direction position)
            (move faces
                  new-face
                  (1- amount)
                  (complex new-face-x new-face-y)
                  new-direction)))))
