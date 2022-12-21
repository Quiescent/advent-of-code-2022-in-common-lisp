(defpackage 2022-day-21
  (:use :cl :iterate :cl-ppcre :metabang-bind :trivia :trivia.ppcre)
  (:shadowing-import-from :arrow-macros :->>))
(in-package 2022-day-21)

(defun part-1 ()
  (let ((input (with-open-file (f (asdf:system-relative-pathname :advent-of-code-2022-in-common-lisp "src/2022-day-21.in"))
                 (streams-lines f))))
    (compute 'root input)))

(defun compute (node graph)
  (let ((current (gethash node graph)))
    (if (numberp current)
        current
        (funcall (car current)
                 (compute (cadr current) graph)
                 (compute (caddr current) graph)))))

(defun streams-lines (f)
  (do ((line (read-line f nil nil) (read-line f nil nil))
       (graph (make-hash-table)))
      ((null line) graph)
    (match line
      ((ppcre "([a-z]+): (-?\\d+)"
              (read monkey-id)
              (read number))
       (setf (gethash monkey-id graph) number))
      ((ppcre "([a-z]+): ([a-z]+) ([-/+*]) ([a-z]+)"
              (read monkey-id)
              (read monkey-1)
              (read operation)
              (read monkey-2))
       (setf (gethash monkey-id graph)
             (list operation monkey-1 monkey-2))))))

(defun get-fresh-graph ()
  (with-open-file (f (asdf:system-relative-pathname :advent-of-code-2022-in-common-lisp "src/2022-day-21.in"))
    (streams-lines f)))

;; Monkey 2: 2407903937671

#+nil
3.09953269128e12

(defun part-2 ()
  (iter
    (with graph = (get-fresh-graph))
    (with (operation monkey-1 monkey-2) = (gethash 'root graph))
    (with monkey-2-result = (compute monkey-2 graph))
    ;; (format t "monkey-2-result: ~a~%" monkey-2-result)
    ;; (finish)
    (for i from 0)
    ;; (setf (gethash 'humn graph) i)
    (for maths = (to-maths-symb monkey-1 graph))
    ;; (format t "Has human: ~a~%" (has-humn maths))
    ;; (format t "Has human l: ~a~%" (has-humn (cadr maths)))
    ;; (format t "Has human r: ~a~%" (has-humn (caddr maths)))
    (format t "(simplify maths): ~a~%" (simplify maths))
    ;; (format t "~A~%" maths)
    (format t "~a~%" (solve (simplify maths) monkey-2-result))
    (finish)
    (for monkey-1-result = (compute monkey-1 graph))
    (format t "monke-y-1-result: ~a~%" monkey-1-result)
    (finish)
    ;; (format t "(list monkey-1-result monkey-2-result): ~a~%" (list monkey-1-result monkey-2-result))
    (when (= monkey-1-result monkey-2-result)
      (return i))
    ))

(defun solve (maths other-monkey)
  (progn
    (format t "other-monkey: ~a~%" other-monkey)
    (format t "maths: ~a~%" maths)
    (if (not (listp maths))
       (progn
         (format t "maths: ~a~%" maths)
         (format t "other-monkey: ~a~%" other-monkey)
         other-monkey)
       (bind (((op l r) maths))
         (cond
           ((has-humn l)
            (case op
              (/ (solve l (* other-monkey r)))
              (+ (solve l (- other-monkey r)))
              (- (solve l (+ other-monkey r)))
              (* (solve l (/ other-monkey r)))))
           (t ;; (has-humn r)
            (case op
              (/ (solve r (* (/ 1 other-monkey) l)))
              (+ (solve r (- other-monkey l)))
              (- (solve r (- (- other-monkey l))))
              (* (solve r (/ other-monkey l))))))))))

;; (defun symbolically (node graph)
;;   (let* ((current (gethash node graph))
;;          (l-arg (cadr current))
;;          (r-arg (caddr current)))
;;     (cond
;;       ((eq node 'humn) 'humn)
;;       ((numberp current) current)
;;       (t (let ((l-result (symbolically l-arg graph))
;;                (r-result (symbolically r-arg graph)))
;;            (if (member 'humn l-result)
;;                ()))))))

(defun simplify (maths)
  (if (not (listp maths))
      maths
      (bind (((op l r) maths))
        (cond
          ((has-humn l) (list op (simplify l) (eval r)))
          ((has-humn r) (list op (eval l) (simplify r)))
          (t (funcall op l r))))))

(defun has-humn (xs)
  (if (listp xs)
      (some (lambda (x)
              (if (listp x)
                  (has-humn x)
                  (eq x 'humn)))
            xs)
      (eq xs 'humn)))

(defun to-maths-symb (node graph)
  (let ((current (gethash node graph)))
    (cond
      ((eq node 'humn) 'humn)
      ((numberp current) current)
      (t (list
          (car current)
          (to-maths-symb (cadr current) graph)
          (to-maths-symb (caddr current) graph))))))

(defun to-maths (node graph)
  (let ((current (gethash node graph)))
    (cond
      ((eq node 'humn) 'humn)
      ((numberp current) current)
      (t (format nil
                 "(~a ~a ~a)"
                 (car current)
                 (to-maths (cadr current) graph)
                 (to-maths (caddr current) graph))))))
