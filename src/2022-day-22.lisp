(defpackage 2022-day-22
  (:use :cl :iterate :cl-ppcre :metabang-bind :trivia :trivia.ppcre)
  (:shadowing-import-from :arrow-macros :->>))
(in-package 2022-day-22)

(defun part-1 ()
  (let ((input (with-open-file (f (asdf:system-relative-pathname :advent-of-code-2022-in-common-lisp "src/2022-day-22.in"))
                 (streams-lines f))))
    input))

(defun streams-lines (f)
  (do ((line (read-line f nil nil) (read-line f nil nil))
       (lines))
      ((null line) (nreverse lines))
    (push line lines)))

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
      ;; Remapped
      (cons (list (face-from 0                 0)
                  (face-from face-length       0)

                  (face-from 0                 face-length)
                  
                  (face-from 0                 (* 2 face-length))
                  (face-from face-length       (* 2 face-length))

                  (face-from 0                 (* 3 face-length)))
            (nth (1+ (* 4 face-length)) lines)))))

;; (defun faces-and-instructions-example (lines)
;;   (bind ((face-length (iter
;;                         (for line in lines)
;;                         (for stripped = (cl-ppcre:regex-replace-all " " line ""))
;;                         (while (not (equal line "")))
;;                         (minimizing (length stripped)))))
;;     (format t "face-length: ~a~%" face-length)
;;     (labels ((face-from (x-offset y-offset)
;;                (iter
;;                  (with face = (make-array (list face-length face-length)
;;                                           :initial-element nil))
;;                  (for y from 0 below face-length)
;;                  (for line in (subseq lines y-offset))
;;                  (iter
;;                    (for square
;;                         in-string (subseq (cl-ppcre:regex-replace-all " "
;;                                                                       line
;;                                                                       "")
;;                                           x-offset))
;;                    (for x from 0 below face-length)
;;                    (when (eq #\# square)
;;                      (setf (aref face y x) t)))
;;                  (finally (return face)))))
;;       (cons (list (face-from 0                 0)

;;                   (face-from 0                 face-length)
;;                   (face-from face-length       face-length)
;;                   (face-from (* 2 face-length) face-length)

;;                   (face-from 0                 (* 2 face-length))
;;                   (face-from face-length       (* 2 face-length)))
;;             (nth (1+ (* 3 face-length)) lines)))))

(defun part-2 ()
  (bind ((input (with-open-file (f (asdf:system-relative-pathname :advent-of-code-2022-in-common-lisp "src/2022-day-22.in"))
                  (streams-lines f)))
         ((faces . instructions) (faces-and-instructions input)))
    (follow-instructions faces instructions)))

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
       (bind ((amount (read-from-string amount-str))
              ((new-face new-direction new-position) (move faces
                                                           face
                                                           amount
                                                           position
                                                           direction)))
         (format t "amount: ~a~%" amount)
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

(defun to-absolute-coord (face-length face x y)
  (case face
    (0 (cons (+ (* 2 face-length) x) y))
    (1 (cons x                       (+ y face-length)))
    (2 (cons (+ x face-length)       (+ y face-length)))
    (3 (cons (+ x (* 2 face-length)) (+ y face-length)))
    (4 (cons (+ x (* 2 face-length)) (+ y (* 2 face-length))))
    (5 (cons (+ x (* 3 face-length)) (+ y (* 2 face-length))))))

;; (defun move (faces face amount position direction)
;;   (if (<= amount 0)
;;       (list face direction position)
;;       (bind ((face-length (array-dimension (car faces) 0))
;;              (x (realpart position))
;;              (y (imagpart position))
;;              (new-position (+ position direction))
;;              (new-x (realpart new-position))
;;              (new-y (imagpart new-position))
;;              ((new-face new-direction new-face-x new-face-y)
;;               (cond
;;                 ;; Right
;;                 ((>= new-x face-length)
;;                  (case face
;;                    (0 (list 5 #c(-1 0) (1- face-length)    (- face-length y 1)))
;;                    (1 (list 2 #c(1 0)  0                   y))
;;                    (2 (list 3 #c(1 0)  0                   y))
;;                    (3 (list 5 #c(0 1)  (- face-length y 1) 0))
;;                    (4 (list 5 #c(1 0)  0                   y))
;;                    (5 (list 0 #c(-1 0) (1- face-length)    (- face-length y 1)))))

;;                 ;; Left
;;                 ((< new-x 0)
;;                  (case face
;;                    (0 (list 2 #c(0 1)  y                   0))
;;                    (1 (list 5 #c(0 1)  (- face-length y 1) (1- face-length)))
;;                    (2 (list 1 #c(-1 0) (1- face-length)    y))
;;                    (3 (list 2 #c(-1 0) (1- face-length)    y))
;;                    (4 (list 2 #c(0 1)  (- face-length y 1) (1- face-length)))
;;                    (5 (list 4 #c(-1 0) (1- face-length)    y))))

;;                 ;; Down
;;                 ((>= new-y face-length)
;;                  (case face
;;                    (0 (list 3 #c(0 1)  x                   0))
;;                    (1 (list 4 #c(0 -1) (- face-length x 1) (1- face-length)))
;;                    (2 (list 4 #c(1 0)  0                   (- face-length x 1)))
;;                    (3 (list 4 #c(0 1)  x                   0))
;;                    (4 (list 1 #c(0 -1) (- face-length x 1) (1- face-length)))
;;                    (5 (list 1 #c(1 0)  0                   (- face-length x 1)))))

;;                 ;; Up
;;                 ((< new-y 0)
;;                  (case face
;;                    (0 (list 1 #c(0 1)  (- face-length x 1) 0))
;;                    (1 (list 0 #c(0 1)  (- face-length x 1) 0))
;;                    (2 (list 0 #c(1 0)  0                   x))
;;                    (3 (list 0 #c(0 -1) x                   (1- face-length)))
;;                    (4 (list 3 #c(0 -1) x                   (1- face-length)))
;;                    (5 (list 3 #c(-1 0) (1- face-length)    (- face-length x 1)))))

;;                 ;; Same face
;;                 (t (list face direction new-x new-y))))
;;              (new-square (aref (nth new-face faces) new-face-y new-face-x)))
;;         (if new-square
;;             (list face direction position)
;;             (move faces
;;                   new-face
;;                   (1- amount)
;;                   (complex new-face-x new-face-y)
;;                   new-direction)))))

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
