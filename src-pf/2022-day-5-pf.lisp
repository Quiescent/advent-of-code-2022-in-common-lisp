(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun shadowing-import-from-fset ()
    `(:shadowing-import-from
      :fset
      ,@(let ((symbols nil))
          (do-external-symbols (symbol (find-package 'fset) symbols)
            (push symbol symbols))))))

(defpackage 2022-day-5-pf
  (:use :cl :iterate :cl-ppcre :metabang-bind :trivia :trivia.ppcre)
  #.(shadowing-import-from-fset)
  (:shadowing-import-from :arrow-macros :->>)
  (:shadowing-import-from :arrow-macros :->))
(in-package 2022-day-5-pf)

(defun to-stacks (crate-lines stack-count)
  (bind (((indices . initial-crates)
          (iter
            (for i from 0 below stack-count)
            (collecting i into indices)
            (collecting (empty-seq) into stacks)
            (finally (return (cons indices (convert 'seq stacks)))))))
    (reduce (lambda (stacks next)
              (reduce (lambda (inner-stacks index)
                        (bind ((crate (aref next (1+ (* 4 index)))))
                          (if (equal crate #\Space)
                              inner-stacks
                              (with inner-stacks
                                    index
                                    (with-first (@ inner-stacks index)
                                      (->> crate
                                        (format nil "~a")
                                        read-from-string))))))
                      indices
                      :initial-value stacks))
            crate-lines
            :initial-value initial-crates)))

(defun read-problem (f)
  (labels ((recur (crate-lines instructions)
             (bind ((line (read-line f nil nil)))
               (cond
                 ;; Done
                 ((null line) instructions)
                 ;; Stack numbers
                 ((and (> (length line) 1)
                       (eq (aref line 1) #\1))
                  (bind ((stack-count (->> (remove ""
                                                   (split " +" line)
                                                   :test #'equal)
                                        length))
                         (stacks (to-stacks crate-lines stack-count)))
                    (cons stacks (recur nil instructions))))
                 ;; Instructions
                 (t (match line
                      ("" (recur nil instructions))
                      ((ppcre "move (\\d+) from (\\d+) to (\\d+)"
                              (read count)
                              (read from)
                              (read to))
                       (recur nil (with-last instructions
                                    (list count (1- from) (1- to)))))
                      (_ (recur (cons line crate-lines)
                                instructions))))))))
    (recur nil (empty-seq))))

(defun part-1 ()
  (with-open-file (f (asdf:system-relative-pathname :advent-of-code-2022-in-common-lisp "src-pf/2022-day-5.in"))
    (bind (((stacks . instructions) (read-problem f)))
      (labels ((execute (instruction stacks)
                 (bind (((count from to) instruction))
                   (-> (with stacks from (subseq (@ stacks from) count))
                     (with to (concat (->> (subseq (@ stacks from) 0 count)
                                        reverse)
                                      (@ stacks to))))))
               (recur (stacks instructions)
                 (if (empty? instructions)
                     (image #'first stacks)
                     (recur (execute (first instructions) stacks)
                            (less-first instructions)))))
        (->> (recur stacks instructions)
          (image #'symbol-name)
          (convert 'list)
          (apply #'concatenate 'string))))))

(defun part-2 ()
  (with-open-file (f (asdf:system-relative-pathname :advent-of-code-2022-in-common-lisp "src-pf/2022-day-5.in"))
    (bind (((stacks . instructions) (read-problem f)))
      (labels ((execute (instruction stacks)
                 (bind (((count from to) instruction))
                   (-> (with stacks from (subseq (@ stacks from) count))
                     (with to (concat (subseq (@ stacks from) 0 count)
                                      (@ stacks to))))))
               (recur (stacks instructions)
                 (if (empty? instructions)
                     (image #'first stacks)
                     (recur (execute (first instructions) stacks)
                            (less-first instructions)))))
        (->> (recur stacks instructions)
          (image #'symbol-name)
          (convert 'list)
          (apply #'concatenate 'string))))))
