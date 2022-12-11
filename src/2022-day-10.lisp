(defpackage 2022-day-10
  (:use :cl :iterate :cl-ppcre :metabang-bind :trivia :trivia.ppcre)
  (:shadowing-import-from :arrow-macros :->>))
(in-package 2022-day-10)

(defun part-1 ()
  (let ((instructions (with-open-file (f (asdf:system-relative-pathname :advent-of-code-2022-in-common-lisp "src/2022-day-10.in"))
                        (do ((line (read-line f nil nil) (read-line f nil nil))
                             (lines nil))
                            ((null line) (nreverse lines))
                          (push line lines)))))
    (labels ((recur (instructions clock register instruction total)
               (let* ((new-total (if (and (>= clock 20)
                                          (= (mod (- clock 20) 40) 0))
                                     (+ total (* clock register))
                                     total))
                      (next-instruction (match (car instructions)
                                          ((ppcre "noop") 'nop)
                                          ((ppcre "addx (-?\\d+)"
                                                  (read amount))
                                           amount))))
                 (cond
                   ((> clock 220) total)
                   ((eq instruction 'nop) (recur (cdr instructions)
                                                 (1+ clock)
                                                 register
                                                 next-instruction
                                                 new-total))
                   (instruction (recur instructions
                                       (1+ clock)
                                       (+ instruction register)
                                       nil
                                       new-total))
                   (t (recur (cdr instructions)
                             (1+ clock)
                             register
                             next-instruction
                             new-total))))))
      (recur instructions 1 1 nil 0))))

;; 720

(defun part-2 ()
  (let ((instructions (with-open-file (f (asdf:system-relative-pathname :advent-of-code-2022-in-common-lisp "src/2022-day-10.in"))
                        (do ((line (read-line f nil nil) (read-line f nil nil))
                             (lines nil))
                            ((null line) (nreverse lines))
                          (push line lines))))
        (image (make-array (list 6 40) :initial-element #\.)))
    (labels ((recur (instructions clock register instruction total)
               (let* ((new-total (if (and (>= clock 20)
                                          (= (mod (- clock 20) 40) 0))
                                     (+ total (* clock register))
                                     total))
                      (next-instruction (match (car instructions)
                                          ((ppcre "noop") 'nop)
                                          ((ppcre "addx (-?\\d+)"
                                                  (read amount))
                                           amount)))
                      (pixel (1- clock)))
                 (let ((pixel (mod pixel 40)))
                   (when (or (and (/= 0 (mod (1- register) 40))
                                  (= (1- pixel) register))
                             (= pixel register)
                             (and (/= 0 (mod register 40))
                                  (= (1+ pixel)  register)))
                     (bind (((:values y x) (floor (1- clock) 40)))
                       (setf (aref image y x) #\#))))
                 (cond
                   ((> clock 240) (dotimes (y 6)
                                    (dotimes (x 40)
                                      (format t "~a" (aref image y x)))
                                    (format t "~%")))
                   ((eq instruction 'nop) (recur (cdr instructions)
                                                 (1+ clock)
                                                 register
                                                 next-instruction
                                                 new-total))
                   (instruction (recur instructions
                                       (1+ clock)
                                       (+ instruction register)
                                       nil
                                       new-total))
                   (t (recur (cdr instructions)
                             (1+ clock)
                             register
                             next-instruction
                             new-total))))))
      (recur instructions 1 1 nil 0))))
