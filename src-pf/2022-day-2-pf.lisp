(defpackage 2022-day-2-pf
  (:use :cl :iterate :cl-ppcre :metabang-bind :trivia :trivia.ppcre)
  (:shadowing-import-from :arrow-macros :->>))
(in-package 2022-day-2-pf)

(defun played-score (my-move)
  (case my-move
    (X 1)
    (Y 2)
    (Z 3)))

(defun translate-move (move)
  (case move
    (A 'rock)
    (B 'paper)
    (C 'scissors)
    (X 'rock)
    (Y 'paper)
    (Z 'scissors)))

(defun win-score (opp-move my-move)
  (match (list (translate-move my-move)
               (translate-move opp-move))
    ;; I pick rock
    ((list 'rock 'rock) 3)
    ((list 'rock 'paper) 0)
    ((list 'rock 'scissors) 6)
    ;; I pick paper
    ((list 'paper 'rock) 6)
    ((list 'paper 'paper) 3)
    ((list 'paper 'scissors) 0)
    ;; I pick scissors
    ((list 'scissors 'rock) 0)
    ((list 'scissors 'paper) 6)
    ((list 'scissors 'scissors) 3)))

(defun part-1 ()
  (with-open-file (f (asdf:system-relative-pathname :advent-of-code-2022-in-common-lisp "src-pf/2022-day-2.in"))
    (labels ((recur (acc)
               (bind ((line (read-line f nil nil)))
                 (if (null line)
                     acc
                     (bind (((opp me) (->> (format nil "(~a)" line)
                                        read-from-string)))
                       (recur (+ acc
                                 (played-score me)
                                 (win-score opp me))))))))
      (recur 0))))

(defun win-score-2 (outcome)
  (case outcome
    (x 0)
    (y 3)
    (z 6)))

(defun translate-outcome (outcome)
  (case outcome
    (x 'lose)
    (y 'draw)
    (z 'win)))

(defun played-score-2 (opp outcome)
  (match (list (translate-move opp)
               (translate-outcome outcome))
    ;; I play rock
    ((list 'paper 'lose) 1)
    ((list 'rock 'draw) 1)
    ((list 'scissors 'win) 1)
    ;; I play paper
    ((list 'paper 'draw) 2)
    ((list 'rock 'win) 2)
    ((list 'scissors 'lose) 2)
    ;; I play scissors
    ((list 'paper 'win) 3)
    ((list 'scissors 'draw) 3)
    ((list 'rock 'lose) 3)))

(defun part-2 ()
  (with-open-file (f (asdf:system-relative-pathname :advent-of-code-2022-in-common-lisp "src-pf/2022-day-2.in"))
    (labels ((recur (acc)
               (bind ((line (read-line f nil nil)))
                 (if (null line)
                     acc
                     (bind (((opp outcome) (->> (format nil "(~a)" line)
                                             read-from-string)))
                       (recur (+ acc
                                 (played-score-2 opp outcome)
                                 (win-score-2 outcome))))))))
      (recur 0))))
