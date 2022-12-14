(defpackage 2022-day-12
  (:use :cl :iterate :cl-ppcre :metabang-bind :trivia :trivia.ppcre)
  (:shadowing-import-from :arrow-macros :->>))
(in-package 2022-day-12)

(defun part-1 ()
  (bind (((map
           (start-x . start-y)
           (end-x . end-y))
          (with-open-file (f (asdf:system-relative-pathname :advent-of-code-2022-in-common-lisp "src/2022-day-12.in"))
               (do ((line (read-line f nil nil) (read-line f nil nil))
                    (lines (list))
                    (start nil)
                    (end nil))
                   ((null line) (let ((output (make-array (list (length lines)
                                                                (length (car lines)))))
                                      (y 0))
                                  (dolist (line (nreverse lines)
                                                (list output start end))
                                    (dotimes (x (length line))
                                      (cond
                                        ((eq #\S (aref line x))
                                         (setf start (cons x y)))
                                        ((eq #\E (aref line x))
                                         (progn
                                           (setf (aref output y x)
                                                 (1+ (- (char-code #\z)
                                                        (char-code #\a))))
                                           (setf end (cons x y))))
                                        (t
                                         (setf (aref output y x)
                                               (- (char-code (aref line x))
                                                  (char-code #\a))))))
                                    (incf y))))
                 (push line lines))))
         (q (datastructures::create-heap :key #'car
                                         :less #'<))
         (dist (make-array (array-dimensions map) :initial-element most-positive-fixnum))
         (parent (make-array (array-dimensions map) :initial-element nil)))
    (datastructures::insert q (list 0 start-x start-y))
    (setf (aref dist start-y start-x) 0)
    (iter
      (for (d x y) = (datastructures::del-min q))
      (for distance = (aref dist y x))
      (when (= d distance)
        (iter
          (for xx
               from (max 0 (1- x))
               to (min (1+ x) (1- (array-dimension map 1))))
          (iter
            (for yy
                 from (max 0 (1- y))
                 to (min (1+ y) (1- (array-dimension map 0))))
            (when (> (+ (abs (- x xx))
                        (abs (- y yy)))
                     1)
              (next-iteration))
            (let ((new-distance (1+ d))
                  (old-distance (aref dist yy xx)))
              (when (and (not (and (= x xx)
                                   (= y yy)))
                         (< new-distance old-distance)
                         (<= (- (aref map yy xx)
                                (aref map y x))
                             1))
                (setf (aref parent yy xx) (cons x y))
                (setf (aref dist yy xx) new-distance)
                (datastructures::insert q (list new-distance xx yy)))))))
      (while (not (datastructures::heap-empty q))))
    (aref dist end-y end-x)))

;; Correct: 484

(defun read-input (f)
  (do ((line (read-line f nil nil) (read-line f nil nil))
       (lines (list))
       (end nil))
      ((null line) (let ((output (make-array (list (length lines)
                                                   (length (car lines)))))
                         (y 0))
                     (dolist (line (nreverse lines)
                                   (list output end))
                       (dotimes (x (length line))
                         (cond
                           ((eq #\S (aref line x))
                            (setf (aref output y x) (- 1)))
                           ((eq #\E (aref line x))
                            (progn
                              (setf (aref output y x)
                                    (1+ (- (char-code #\z)
                                           (char-code #\a))))
                              (setf end (cons x y))))
                           (t
                            (setf (aref output y x)
                                  (- (char-code (aref line x))
                                     (char-code #\a))))))
                       (incf y))))
    (push line lines)))

(defun part-2 ()
  (bind (((map (end-x . end-y))
          (with-open-file (f (asdf:system-relative-pathname :advent-of-code-2022-in-common-lisp "src/2022-day-12.in"))
               (read-input f))))
    (labels ((search-from (start-x start-y)
               (let ((q (datastructures::create-heap :key #'car
                                                     :less #'<))
                     (dist (make-array (array-dimensions map) :initial-element most-positive-fixnum))
                     (parent (make-array (array-dimensions map) :initial-element nil)))
                 (datastructures::insert q (list 0 start-x start-y))
                 (setf (aref dist start-y start-x) 0)
                 (iter
                   (for (d x y) = (datastructures::del-min q))
                   (for distance = (aref dist y x))
                   (when (= d distance)
                     (iter
                       (for xx
                            from (max 0 (1- x))
                            to (min (1+ x) (1- (array-dimension map 1))))
                       (iter
                         (for yy
                              from (max 0 (1- y))
                              to (min (1+ y) (1- (array-dimension map 0))))
                         (when (> (+ (abs (- x xx))
                                     (abs (- y yy)))
                                  1)
                           (next-iteration))
                         (let ((new-distance (1+ d))
                               (old-distance (aref dist yy xx)))
                           (when (and (not (and (= x xx)
                                                (= y yy)))
                                      (< new-distance old-distance)
                                      (<= (- (aref map yy xx)
                                             (aref map y x))
                                          1))
                             (setf (aref parent yy xx) (cons x y))
                             (setf (aref dist yy xx) new-distance)
                             (datastructures::insert q (list new-distance xx yy)))))))
                   (while (not (datastructures::heap-empty q))))
                 (aref dist end-y end-x))))
      (let ((minimum most-positive-fixnum))
        (dotimes (x (array-dimension map 1) minimum)
          (dotimes (y (array-dimension map 0))
            (when (= 0 (aref map y x))
              (setf minimum (min minimum (search-from x y))))))))))

(defvar deltas '((1 0) (0 1) (-1 0) (0 -1)))

(defun pure-queue ()
  (cons nil nil))

(defun pure-queue-enqueue (xs x)
  (bind (((front . end) xs))
    (cons front (cons x end))))

(defun pure-queue-dequeue (xs)
  (bind (((front . end) xs))
    (if (null front)
        (let ((new-front (reverse end)))
          (values (car new-front)
                  (cons (cdr new-front) nil)))
        (values (car front)
                (cons (cdr front) end)))))

(defun search-from (map seen q dist)
  (bind (((:values coord popped-q) (pure-queue-dequeue q))
         ((x . y) coord))
    (if (eq (aref map y x)
            (1+ (- (char-code #\z)
                   (char-code #\a))))
        (fset:@ dist (fset:seq x y))
        (bind ((go-to
                (->> (mapcar
                      (lambda (delta)
                        (bind (((dx dy) delta)
                               (xx (+ x dx))
                               (yy (+ y dy)))
                          (when (and (>= xx 0)
                                     (< xx (array-dimension map 1))
                                     (>= yy 0)
                                     (< yy (array-dimension map 0))
                                     (<= (- (aref map yy xx)
                                            (aref map y x))
                                         1)
                                     (not (fset:contains?
                                           seen
                                           (fset:seq xx yy))))
                            (cons xx yy))))
                      deltas)
                  (remove nil)))
               (new-seen (reduce (lambda (new-seen new-coord)
                                   (fset:with new-seen
                                              (fset:seq (car new-coord)
                                                        (cdr new-coord))))
                                 go-to
                                 :initial-value seen))
               (new-q (reduce (lambda (new-queue new-coord)
                                (pure-queue-enqueue new-queue new-coord))
                              go-to
                              :initial-value popped-q))
               (old-d (fset:@ dist (fset:seq x y)))
               (new-dist (reduce (lambda (new-dist new-coord)
                                   (fset:with new-dist
                                              (fset:seq (car new-coord)
                                                        (cdr new-coord))
                                              (1+ old-d)))
                                 go-to
                                 :initial-value dist)))
          (search-from map new-seen new-q new-dist)))))

(defun part-2-alt ()
  (bind (((map (end-x . end-y))
          (with-open-file (f (asdf:system-relative-pathname :advent-of-code-2022-in-common-lisp "src/2022-day-12.in"))
            (read-input f))))
    (search-from map
                 (fset:set (fset:seq 0 0))
                 (pure-queue-enqueue (pure-queue)
                                     (cons 0 0))
                 (fset:with (fset:map) (fset:seq 0 0) 0))
    ;; (iter
    ;;   (for y from 0 below (array-dimension map 0))
    ;;   (minimizing
    ;;    (iter
    ;;      (for x from 0 below (array-dimension map 1))
    ;;      (when (= 0 (aref map y x))
    ;;        (minimizing (search-from (fset:set)
    ;;                                 (pure-queue-enqueue (pure-queue)
    ;;                                                     (cons x y))
    ;;                                 0))))))
    ))

;; Correct: 478
