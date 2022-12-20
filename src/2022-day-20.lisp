(defpackage 2022-day-20
  (:use :cl :iterate :cl-ppcre :metabang-bind :trivia :trivia.ppcre)
  (:shadowing-import-from :arrow-macros :->>)
  (:shadowing-import-from :arrow-macros :->))
(in-package 2022-day-20)

(defun part-1 ()
  (bind ((input (with-open-file (f (asdf:system-relative-pathname :advent-of-code-2022-in-common-lisp "src/2022-day-20.in"))
                  (input-lines f)))
         ((order . reverse-mapping) (symbols-for-numbers input)))
    (apply #'+
           (coordinates (mix-2 order order reverse-mapping)
                        reverse-mapping))))

;; Wrong: 8494

(defun coordinates (order reverse-mapping)
  (let* ((zero (iter
                 (for (s value) in-hashtable reverse-mapping)
                 (when (= value 0)
                   (return s))))
         (zero-pos (position zero order))
         (numbers (numbers-for-symbols order reverse-mapping))
         (len (length order)))
    (print (list (nth (mod (+ zero-pos 1000) len) numbers)
                 (nth (mod (+ zero-pos 2000) len) numbers)
                 (nth (mod (+ zero-pos 3000) len) numbers)))))

#+nil
(iter
  (with len = (length order))
  (with zero =
        (iter
          (for (s value) in-hashtable reverse-mapping)
          (when (= value 0)
            (return s))))
  (with d-numbers = (from-list order))
  (for s = (d-list-value d-numbers))
  (while (not (eq s zero)))
  (setf d-numbers (forward 1 d-numbers))
  (finally
   (return
     (let ((numbers (numbers-for-symbols (to-list d-numbers) reverse-mapping)))
       (list (nth (mod 1000 len) numbers)
             (nth (mod 2000 len) numbers)
             (nth (mod 3000 len) numbers))))))

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

(defun mix-2 (order original-order reverse-map)
  (iter
    (with xs = (copy-seq order))
    (with len = (length order))
    (for next in original-order)
    (for idx = (position next xs))
    (for without-next = (concatenate 'list
                                     (subseq xs 0 idx)
                                     (subseq xs (1+ idx))))
    (for amount = (gethash next reverse-map))
    (for new-idx = (mod (+ idx amount)
                        (1- len)))
    (if (= new-idx 0)
        (setf xs (cons next without-next))
        (setf xs (concatenate 'list
                              (subseq without-next 0 new-idx)
                              (list next)
                              (subseq without-next new-idx))))
    (finally (return xs))))

(defun mix (order original-order reverse-map)
  (iter
    (with d-numbers = (from-list order))
    (for next in original-order)
    (for amount = (gethash next reverse-map))
    (iter
      (for s = (d-list-value d-numbers))
      (while (not (eq s next)))
      (setf d-numbers (forward 1 d-numbers)))
    (setf d-numbers (d-list-pop d-numbers))
    (setf d-numbers (if (< amount 0)
                        (back (1+ (abs amount)) d-numbers)
                        (forward (1- amount) d-numbers)))
    (setf d-numbers (d-list-insert d-numbers next))
    (iter
      (for s = (d-list-value d-numbers))
      (while (not (eq s next)))
      (setf d-numbers (forward 1 d-numbers)))
    (print (numbers-for-symbols (to-list d-numbers) reverse-map))
    (finally
     (return (to-list d-numbers)))))

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
  (bind ((input (with-open-file (f (asdf:system-relative-pathname :advent-of-code-2022-in-common-lisp "src/2022-day-20.in"))
                  (mapcar (lambda (x) (* x 811589153))
                          (input-lines f))))
         ((order . reverse-mapping) (symbols-for-numbers input)))
    ;; (mix-2 order reverse-mapping)
    (apply #'+
           (coordinates (-> (mix-2 order order reverse-mapping)
                          (mix-2 order reverse-mapping)
                          (mix-2 order reverse-mapping)
                          (mix-2 order reverse-mapping)
                          (mix-2 order reverse-mapping)
                          (mix-2 order reverse-mapping)
                          (mix-2 order reverse-mapping)
                          (mix-2 order reverse-mapping)
                          (mix-2 order reverse-mapping)
                          (mix-2 order reverse-mapping))
                        reverse-mapping))))

;; Wrong: 6293062292362
