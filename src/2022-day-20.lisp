(defpackage 2022-day-20
  (:use :cl :iterate :cl-ppcre :metabang-bind :trivia :trivia.ppcre)
  (:shadowing-import-from :arrow-macros :->>))
(in-package 2022-day-20)

(defun part-1 ()
  (bind ((input (with-open-file (f (asdf:system-relative-pathname :advent-of-code-2022-in-common-lisp "src/2022-day-20.in"))
                  (input-lines f)))
         ((order . reverse-mapping) (symbols-for-numbers input)))
    (numbers-for-symbols (mix order reverse-mapping)
                         reverse-mapping)))

(defun symbols-for-numbers (xs)
  (iter
    (with result = (make-hash-table))
    (for x in xs)
    (for s = (gensym (format nil "~a" x)))
    (setf (gethash s result) x)
    (collecting s into order)
    (finally
     (return (cons order result)))))

(defun numbers-for-symbols (ss reverse-mapping)
  (iter
    (for s in ss)
    (collecting (gethash s reverse-mapping))))

(defun mix (order reverse-map)
  (iter
    (with d-numbers = (from-list order))
    (for next in order)
    (format t "next: ~a~%" next)
    (for amount = (gethash next reverse-map))
    (format t "amount: ~a~%" amount)
    (iter
      (for s = (d-list-value d-numbers))
      (while (not (eq s next)))
      (summing 1 into offset)
      (setf d-numbers (forward 1 d-numbers)))
    (summing amount into offset)
    (setf d-numbers (if (< amount 0)
                        (back (1+ (abs amount)) d-numbers)
                        (forward amount d-numbers)))
    (setf d-numbers (d-list-insert d-numbers next))
    (setf d-numbers (forward 1 d-numbers))
    (iter
      (for s = (d-list-value d-numbers))
      (while (not (eq s next)))
      (summing 1 into offset)
      (setf d-numbers (forward 1 d-numbers)))
    (print-d-list d-numbers)
    (format t "Popping: ~a~%" (d-list-value d-numbers))
    (print-d-list d-numbers)
    ;; (let ((*print-circle* t))
    ;;   (print d-numbers))
    (summing -1 into offset)
    (setf d-numbers (d-list-pop d-numbers))
    (print-d-list d-numbers)
    (format t "After a round: ~a~%~%" (numbers-for-symbols (to-list d-numbers)
                                                         reverse-map))
    (finally
     (return (to-list (forward offset d-numbers))))))

(defstruct d-list
  (value nil)
  (forward nil)
  (back nil))

(defun from-list (xs)
  (iter
    (with result = (create-d-list (car xs)))
    (for x in (cdr xs))
    (setf result (d-list-insert result x))
    (finally
     (return (d-list-forward result)))))

(defun to-list (xs)
  (iter
    (with ys = xs)
    (collecting (d-list-value ys))
    (setf ys (forward 1 ys))
    (while (not (eq ys xs)))))

(defun forward (n xs)
  (dotimes (i n xs)
    (setf xs (d-list-forward xs))))

(defun back (n xs)
  (dotimes (i n xs)
    (setf xs (d-list-back xs))))

(defun d-list-insert (xs x)
  (let ((inserted (make-d-list :value x
                               :forward (d-list-forward xs)
                               :back xs)))
    (setf (d-list-back (d-list-forward xs)) inserted
          (d-list-forward xs)               inserted)
    inserted))

(defun d-list-pop (xs)
  (prog1 (d-list-forward xs)
    (setf (d-list-forward (d-list-back xs))
          (d-list-forward xs))
    (setf (d-list-back (d-list-forward xs))
          (d-list-back xs))))

(defun print-d-list (xs)
  (format t "(")
  (iter
    (with ys = xs)
    (format t "~a " (d-list-value ys))
    (setf ys (forward 1 ys))
    (while (not (eq ys xs))))
  (format t ")~%"))

(defun create-d-list (first-elem)
  (let ((xs (make-d-list :value first-elem
                         :forward nil
                         :back nil)))
    (setf (d-list-forward xs) xs
          (d-list-back xs)    xs)
    xs))

(defun input-lines (f)
  (do ((line (read-line f nil nil) (read-line f nil nil))
       (lines))
      ((null line) (nreverse lines))
    (push (read-from-string line) lines)))

(defun part-2 ()
  (let ((input (with-open-file (f (asdf:system-relative-pathname :advent-of-code-2022-in-common-lisp "src/2022-day-20.in"))
                 )))
    ))
