(defpackage 2022-day-8
  (:use :cl :iterate :cl-ppcre :metabang-bind :trivia :trivia.ppcre)
  (:shadowing-import-from :arrow-macros :->>)
  (:shadowing-import-from :arrow-macros :->))
(in-package 2022-day-8)

(defun part-1 ()
  (let* ((input (with-open-file (f (asdf:system-relative-pathname :advent-of-code-2022-in-common-lisp "src/2022-day-8.in"))
                  (do ((line (read-line f nil nil) (read-line f nil nil))
                       (lines nil))
                      ((null line) (coerce (nreverse lines) 'vector))
                    (push line lines))))
         (visible (make-hash-table :test #'equal)))
    (iter
      (for y from 0 below (length input))
      (for row = (aref input y))
      (iter
        (for x from 0 below (1- (length (aref input 0))))
        (for tree = (->> (aref row x) digit-char-p))
        (with previous-trees = (list))
        (when (every (lambda (other-tree) (< other-tree tree)) previous-trees)
          (setf (gethash (cons x y) visible) t))
        (push tree previous-trees))
      (iter
        (for x from (1- (length (aref input 0))) downto 0)
        (for tree = (->> (aref row x) digit-char-p))
        (with previous-trees = (list))
        (when (every (lambda (other-tree) (< other-tree tree)) previous-trees)
          (setf (gethash (cons x y) visible) t))
        (push tree previous-trees)))
    (iter
      (for x from 0 below (1- (length (aref input 0))))
      (iter
        (for y from 0 below (length input))
        (for tree = (-> (aref input y) (aref x) digit-char-p))
        (with previous-trees = (list))
        (when (every (lambda (other-tree) (< other-tree tree)) previous-trees)
          (setf (gethash (cons x y) visible) t))
        (push tree previous-trees))
      (iter
        (for y from (1- (length input)) downto 0)
        (for tree = (-> (aref input y) (aref x) digit-char-p))
        (with previous-trees = (list))
        (when (every (lambda (other-tree) (< other-tree tree)) previous-trees)
          (setf (gethash (cons x y) visible) t))
        (push tree previous-trees)))
    (hash-table-count visible)))

;; 597, 753

(defun part-2 ()
  (let* ((input (with-open-file (f (asdf:system-relative-pathname :advent-of-code-2022-in-common-lisp "src/2022-day-8.in"))
                  (do ((line (read-line f nil nil) (read-line f nil nil))
                       (lines nil))
                      ((null line) (coerce (nreverse lines) 'vector))
                    (push line lines)))))
    (iter
      (for y from 0 below (length input))
      (maximizing
       (iter
         (for x from 0 below (length (aref input 0)))
         (for current-tree = (-> (aref input y) (aref x) digit-char-p))
         (maximizing
          (* (iter
               (for xl from (1- x) downto 0)
               (for next-tree = (-> (aref input y) (aref xl) digit-char-p))
               (counting t)
               (when (>= next-tree current-tree)
                 (finish)))
             (iter
               (for xr from (1+ x) below (length (aref input 0)))
               (for next-tree = (-> (aref input y) (aref xr) digit-char-p))
               (counting t)
               (when (>= next-tree current-tree)
                 (finish)))
             (iter
               (for yu from (1- y) downto 0)
               (for next-tree = (-> (aref input yu) (aref x) digit-char-p))
               (counting t)
               (when (>= next-tree current-tree)
                 (finish)))
             (iter
               (for yd from (1+ y) below (length input))
               (for next-tree = (-> (aref input yd) (aref x) digit-char-p))
               (counting t)
               (when (>= next-tree current-tree)
                 (finish))))))))))
