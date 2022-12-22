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
  (bind ((face-length (->> (cl-ppcre:regex-replace-all " " (car lines) "")
                        length)))
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
            (nth (1+ (* 3 face-length)) lines)))))

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
         (setf face      new-face
               direction new-direction
               position  new-position)))
      ((ppcre "^([LR])"
              (read turn))
       (format t "turn: ~a~%" turn)
       (incf i)
       (case turn
         (L (setf direction (* direction #c(0 -1))))
         (R (setf direction (* direction #c(0 1)))))))))

(defun move (faces face amount position direction)
  (if (<= amount 0)
      (list face direction position)
      (bind ((face-length (array-dimension (car faces) 0))
             (current-face (nth face faces))
             (new-position (+ position direction))
             (new-x (realpart new-position))
             (new-y (imagpart new-position)))
        (cond
          ((>= new-x face-length)
           (bind (((new-face . new-direction)
                   (case face
                     (0 (cons 5 #c(-1 0)))
                     (1 (cons 2 #c(1 0)))
                     (2 (cons 3 #c(1 0)))
                     (3 (cons 5 #c(0 1)))
                     (4 (cons 5 #c(1 0)))
                     (5 (cons 0 #c(-1 0))))))))

          ((< new-x 0)
           (bind (((new-face . new-direction)
                   (case face
                     (0 (cons 2 #c(0 1)))
                     (1 (cons 5 #c(0 1)))
                     (2 (cons 1 #c(-1 0)))
                     (3 (cons 2 #c(-1 0)))
                     (4 (cons 2 #c(0 1)))
                     (5 (cons 4 #c(-1 0))))))))

          ((>= new-y face-length)
           (bind (((new-face . new-direction)
                   (case face
                     (0 (cons 3 #c(0 1)))
                     (1 (cons 4 #c(0 -1)))
                     (2 (cons 4 #c(1 0)))
                     (3 (cons 4 #c(0 1)))
                     (4 (cons 1 #c(0 -1)))
                     (5 (cons 1 #c(1 0))))))))

          ((< new-y 0)
           (bind (((new-face . new-direction)
                   (case face
                     (0 (cons 1 #c(0 1)))
                     (1 (cons 0 #c(0 1)))
                     (2 (cons 0 #c(1 0)))
                     (3 (cons 0 #c(0 -1)))
                     (4 (cons 3 #c(0 -1)))
                     (5 (cons 3 #c(-1 0))))))))))))
