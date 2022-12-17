(defpackage 2022-day-17
  (:use :cl :iterate :cl-ppcre :metabang-bind :trivia :trivia.ppcre)
  (:shadowing-import-from :arrow-macros :->>))
(in-package 2022-day-17)

(defvar rocks
  '(((t t t t))

    ((nil t nil)
     (t t t)
     (nil t nil))

    ((nil nil t)
     (nil nil t)
     (t t t))

    ((t)
     (t)
     (t)
     (t))

    ((t t)
     (t t))))

(defun part-1 ()
  (let ((directions (with-open-file (f (asdf:system-relative-pathname :advent-of-code-2022-in-common-lisp "src/2022-day-17.in"))
                 (read-line f nil nil))))
    (highest-rock directions)))

(defun highest-rock (directions)
  (let ((height 0)
        (placements (make-hash-table :test #'equal))
        (gas 0))
    (labels ((collides (block new-x new-y)
               (iter
                 (for row in block)
                 (for yy from 0)
                 (thereis
                  (iter
                    (for square in row)
                    (for xx from 0)
                    (thereis (and square
                                  (gethash (cons (+ new-x xx)
                                                 (- new-y yy))
                                           placements))))))))
      (dotimes (i 2022 (nth 3 (bounds placements)))
        (iter
          (with x = 2)
          (with block = (nth (mod i (length rocks)) rocks))
          (with y = (if (= i 0)
                        (+ height 3 (length block))
                        (+ height 4 (length block))))
          (for dir = (aref directions gas))
          ;; (format t "(cons x y): ~a~%" (cons x y))
          (cond
            ((eq dir #\<)
             (let ((new-x (1- x)))
               (when (and (not (< new-x 0))
                          (not (collides block new-x y)))
                 ;; (format t "Bumped x: ~a~%" new-x)
                 (setf x new-x))))
            ((eq dir #\>)
             (let ((new-x (1+ x)))
               (when (and (not (> (+ new-x (length (car block)))
                                  7))
                          (not (collides block new-x y)))
                 ;; (format t "Bumped x: ~a~%" new-x)
                 (setf x new-x)))))
          (setf gas (mod (1+ gas) (length directions)))
          (for new-y = (1- y))
          ;; (format t "new-y: ~a~%" new-y)
          (if (or (< (- new-y (length block)) 0)
                  (collides block x new-y))
              (progn
                (iter
                  (for row in block)
                  (for yy from y downto 0)
                  (iter
                    (for square in row)
                    (for xx from x)
                    (when square
                      (setf (gethash (cons xx yy) placements) t))))
                (setf height (max height new-y))
                (print (nth 3 (bounds placements)))
                ;; (format t "Placed!: ~a~%" (cons x y))
                (finish))
              (setf y new-y)))
        ;; (format t "~%")
        ;; (print-grid placements)
        ;; (format t "~%")
        ))))

(defun bounds (grid)
  (iter
    (for (key value) in-hashtable grid)
    (for (x . y) = key)
    (minimizing x into min-x)
    (maximizing x into max-x)
    (minimizing y into min-y)
    (maximizing y into max-y)
    (finally
     (return (list min-x min-y max-x max-y)))))

(defun print-grid (grid)
  (bind (((min-x min-y max-x max-y) (bounds grid)))
    (iter
      (for y from max-y downto min-y)
      (iter
        (for x from 0 to 6)
        (if (gethash (cons x y) grid)
            (format t "#")
            (format t ".")))
      (format t "~%"))))

(defun part-2 ()
  (let ((directions (with-open-file (f (asdf:system-relative-pathname :advent-of-code-2022-in-common-lisp "src/2022-day-17.in"))
                 (read-line f nil nil))))
    (highest-rock-2 directions)))

(defun highest-rock-2 (directions)
  (let ((height 0)
        (placements (make-hash-table :test #'equal))
        (gas 0)
        (heights nil))
    (labels ((collides (block new-x new-y)
               (iter
                 (for row in block)
                 (for yy from 0)
                 (thereis
                  (iter
                    (for square in row)
                    (for xx from 0)
                    (thereis (and square
                                  (gethash (cons (+ new-x xx)
                                                 (- new-y yy))
                                           placements))))))))
      (dotimes (i 2050 (nth 3 (bounds placements)))
        (iter
          (with x = 2)
          (with block = (nth (mod i (length rocks)) rocks))
          (with y = (if (= i 0)
                        (+ height 3 (length block))
                        (+ height 4 (length block))))
          (for dir = (aref directions gas))
          ;; (format t "(cons x y): ~a~%" (cons x y))
          (cond
            ((eq dir #\<)
             (let ((new-x (1- x)))
               (when (and (not (< new-x 0))
                          (not (collides block new-x y)))
                 ;; (format t "Bumped x: ~a~%" new-x)
                 (setf x new-x))))
            ((eq dir #\>)
             (let ((new-x (1+ x)))
               (when (and (not (> (+ new-x (length (car block)))
                                  7))
                          (not (collides block new-x y)))
                 ;; (format t "Bumped x: ~a~%" new-x)
                 (setf x new-x)))))
          (setf gas (mod (1+ gas) (length directions)))
          (for new-y = (1- y))
          ;; (format t "new-y: ~a~%" new-y)
          (if (or (< (- new-y (length block)) 0)
                  (collides block x new-y))
              (progn
                (iter
                  (for row in block)
                  (for yy from y downto 0)
                  (iter
                    (for square in row)
                    (for xx from x)
                    (when square
                      (setf (gethash (cons xx yy) placements) t))))
                (setf height (max height new-y))
                (push (nth 3 (bounds placements)) heights)
                ;; (format t "Placed!: ~a~%" (cons x y))
                (finish))
              (setf y new-y)))
        ;; (format t "~%")
        ;; (print-grid placements)
        ;; (format t "~%")
        )
      (bind ((ordered-heights (reverse heights))
             (diffs (iter
                      (for height in ordered-heights)
                      (for p-height previous height)
                      (when p-height
                        (print (- height p-height))
                        (collecting (- height p-height)))))
             ((best-length index gain)
              (iter
                (for xss on diffs)
                (for index from 0)
                (for (gain . best-length) =
                     (iter
                       (for i from 0 below (floor (length diffs) 2))
                       (for xs = (subseq xss 0 i))
                       (for repeats = (search xs xss :start2 (length xs)))
                       (when (not repeats)
                         (finish))
                       (finding (cons (apply #'+ xs) i)
                                maximizing i)))
                (finding (list best-length index gain) maximizing best-length))))
        (print (nth 2021 ordered-heights))
        (+ (nth index ordered-heights)
           (* gain (floor (- 2022 index) best-length)))))))

;; By inspecting example output
;; repeat is:
(defvar repeat-example '(1 
                         2 
                         3 
                         0 
                         1 
                         1 
                         3 
                         2 
                         2 
                         0 
                         0 
                         2 
                         3 
                         4 
                         0 
                         1 
                         2 
                         1 
                         2 
                         0 
                         1 
                         2 
                         1 
                         2 
                         0 
                         1 
                         3 
                         2 
                         0 
                         0 
                         1 
                         3 
                         3 
                         4 
                         0))
;; It starts at index 19
;;
;; It adds:
(apply #'+ repeat-example)
;; 53
