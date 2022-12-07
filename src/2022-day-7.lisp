(defpackage 2022-day-7
  (:use :cl :iterate :cl-ppcre :metabang-bind :trivia :trivia.ppcre)
  (:shadowing-import-from :arrow-macros :->>))
(in-package 2022-day-7)

(defun part-1 ()
  (let ((input (with-open-file (f (asdf:system-relative-pathname :advent-of-code-2022-in-common-lisp "src/2022-day-7.in"))
                 (do ((line (read-line f nil nil) (read-line f nil nil))
                      (graph (make-hash-table :test #'equal))
                      (pwd (list "/"))
                      (listing nil))
                     ((null line) graph)
                   (match line
                     ((ppcre "\\$ cd (.*)"
                             where)
                      (cond
                        ((string-equal "/" where) (setf pwd (list "/")))
                        ((string-equal ".." where) (pop pwd))
                        (t  (push where pwd)))
                      (setf listing nil))
                     ((ppcre "\\$ ls")
                      (setf listing t))
                     ((ppcre "(\\d+) (\\w+)"
                             (read file-size)
                             (read file-name))
                      (iter
                        (for subdir on pwd)
                        (incf (gethash subdir graph 0) file-size)))
                     ((ppcre "dir (\\w+)"
                             (read dir-name))
                      nil))))))
    (iter
      (for (key size) in-hashtable input)
      (when (<= size 100000)
        (summing size)))))

(defun part-2 ()
  (let ((input (with-open-file (f (asdf:system-relative-pathname :advent-of-code-2022-in-common-lisp "src/2022-day-7.in"))
                 (do ((line (read-line f nil nil) (read-line f nil nil))
                      (graph (make-hash-table :test #'equal))
                      (pwd (list "/"))
                      (listing nil))
                     ((null line) graph)
                   (match line
                     ((ppcre "\\$ cd (.*)"
                             where)
                      (cond
                        ((string-equal "/" where) (setf pwd (list "/")))
                        ((string-equal ".." where) (pop pwd))
                        (t  (push where pwd)))
                      (setf listing nil))
                     ((ppcre "\\$ ls")
                      (setf listing t))
                     ((ppcre "(\\d+) (\\w+)"
                             (read file-size)
                             (read file-name))
                      (iter
                        (for subdir on pwd)
                        (incf (gethash subdir graph 0) file-size)))
                     ((ppcre "dir (\\w+)"
                             (read dir-name))
                      nil))))))
    (let* ((available (- 70000000 (gethash (list "/") input)))
           (required (- 30000000 available)))
      (iter
        (for (key size) in-hashtable input)
        (when (>= size required)
          (minimizing size))))))
