(defpackage 2022-day-25
  (:use :cl :iterate :cl-ppcre :metabang-bind :trivia :trivia.ppcre)
  (:shadowing-import-from :arrow-macros :->>))
(in-package 2022-day-25)

(defun part-1 ()
  (bind ((input (with-open-file (f (asdf:system-relative-pathname :advent-of-code-2022-in-common-lisp "src/2022-day-25.in"))
                  (streams-lines f))))
    (iter
      (for line in input)
      (summing (to-decimal line) into dec-result)
      (finally
       (return (to-snafu-2 dec-result))))))

(defun to-snafu-2 (x)
  (bind ((upper (iter
                  (for i from 0)
                  (for number = (iter
                                  (for j from 0 below i)
                                  (collecting #\2 :result-type string)))
                  (when (> (to-decimal number) x)
                    (return number)))))
    (iter
      (with current = (copy-seq upper))
      (for idx from 0 below (length upper))
      (setf current (iter
                      (for option in '(#\= #\- #\0 #\1 #\2))
                      (for new-number = (copy-seq current))
                      (setf (aref new-number idx) option)
                      (when (>= (to-decimal new-number) x)
                        (return new-number))))
      (format t "current: ~a~%" current)
      (when (= (to-decimal current) x)
        (return current))
      (finally (return current)))))

;; Dec: 33979178787567
(defun to-snafu (x)
  (labels ((recur (xs)
             (iter
               (for digit in '("2" "1" "-" "=" "0"))
               (for new-number = (concatenate 'string digit xs))
               ;; (format t "new-number: ~a~%" new-number)
               (for remaining-digits = (- 21 (length new-number)))
               (for max-added = (iter
                                  (for i from 0 below remaining-digits)
                                  (collecting #\2 :result-type string)))
               (for upper = (->> (concatenate 'string max-added xs)
                              to-decimal))
               (when (< upper x)
                 (finish))
               (for max-removed = (iter
                                    (for i from 0 below remaining-digits)
                                    (collecting #\= :result-type string)))
               (for lower = (->> (concatenate 'string max-removed xs)
                              to-decimal))
               (when (> lower x)
                 (finish))
               (for dec = (to-decimal new-number))
               ;; (format t "dec: ~a~%" dec)
               (thereis
                (cond
                  ((> (length new-number) 20) nil)
                  ((= dec x) new-number)
                  ((< dec x) (recur new-number))
                  ((> dec x) nil))))))
    (recur "")))

(defun to-decimal (s)
  (iter
    (for c in-string (reverse s))
    (for i from 0)
    (for mult = (expt 5 i))
    (summing (* (cond
                  ((eq c #\1) 1)
                  ((eq c #\2) 2)
                  ((eq c #\0) 0)
                  ((eq c #\-) -1)
                  ((eq c #\=) -2))
                mult))))

(defun streams-lines (f)
  (do ((line (read-line f nil nil) (read-line f nil nil))
       (lines))
      ((null line) (nreverse lines))
    (push line lines)))
