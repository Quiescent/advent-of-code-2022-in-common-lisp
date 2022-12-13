(defpackage 2022-day-13
  (:use :cl :iterate :cl-ppcre :metabang-bind :trivia :trivia.ppcre)
  (:shadowing-import-from :arrow-macros :->>)
  (:shadowing-import-from :arrow-macros :->))
(in-package 2022-day-13)

(defun part-1 ()
  (let ((input (with-open-file (f (asdf:system-relative-pathname :advent-of-code-2022-in-common-lisp "src/2022-day-13.in"))
                 (do ((line (read-line f nil nil) (read-line f nil nil))
                      (pairs nil)
                      (current-pair nil))
                     ((null line) (nreverse pairs))
                   (if (equal line "")
                       (progn
                         (push (nreverse current-pair) pairs)
                         (setf current-pair nil))
                       (push (->> (regex-replace-all ","
                                                 (regex-replace-all "\\["
                                                                    (regex-replace-all "\\]" line ")")
                                                                    "(")
                                                 " ")
                               read-from-string)
                             current-pair))))))
    (apply #'+
           (iter
             (for i from 1)
             (for (left right) in input)
             (when (is-less left right)
               (collecting i))))))

(defun is-less (left right)
  (cond
    ((and (null left)
          (not (null right)))
     t)
    ((and (null right)
          (null left))
     'same)
    ((null right) nil)
    (t (bind (((x . xs) left)
              ((y . ys) right))
         (cond
           ;; Comparing integers
           ((and (numberp x)
                 (numberp y))
            (and (not (> x y))
                 (or (< x y)
                     (is-less xs ys))))
           ;; Both are lists
           ((and (listp x)
                 (listp y))
            (let ((res (is-less x y)))
              (if (eq res 'same)
                  (is-less xs ys)
                  res)))
           ;; x is integer y is list
           ((and (numberp x)
                 (listp y))
            (let ((res (is-less (list x) y)))
              (if (eq res 'same)
                  (is-less xs ys)
                  res)))
           ;; y is integer x is list
           ((and (listp x)
                 (numberp y))
            (let ((res (is-less x (list y))))
              (if (eq res 'same)
                  (is-less xs ys)
                  res)))
           (t (let ((res (is-less x y)))
                (if (eq res 'same)
                    (is-less xs ys)
                    res))))))))

;; Wrong: 3260
;; Wrong: 780

(defun part-2 ()
  (let* ((input (with-open-file (f (asdf:system-relative-pathname :advent-of-code-2022-in-common-lisp "src/2022-day-13.in"))
                  (do ((line (read-line f nil nil) (read-line f nil nil))
                       (signals nil))
                      ((null line) signals)
                    (when (not (equal line ""))
                      (push (->> (regex-replace-all ","
                                                    (regex-replace-all "\\["
                                                                       (regex-replace-all "\\]" line ")")
                                                                       "(")
                                                    " ")
                              read-from-string)
                            signals)))))
         (sig-1 (list (list 2)))
         (sig-2 (list (list 6)))
         (sorted (sort (cons sig-1
                             (cons sig-2
                                   input))
                       #'is-less)))
    (* (1+ (position sig-1 sorted))
       (1+ (position sig-2 sorted)))))
